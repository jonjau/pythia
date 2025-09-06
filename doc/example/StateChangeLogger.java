package org.example;

import org.junit.jupiter.api.extension.*;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.CookieManager;
import java.time.Instant;
import java.util.*;
import java.nio.charset.StandardCharsets;
import java.net.URLEncoder;
import java.lang.reflect.Method;

import myorg.dbclient.Database;

public class StateChangeLogger implements BeforeTestExecutionCallback, AfterTestExecutionCallback {
  private static final String BASE_URL = "http://localhost:3000";
  private static final String FACTS_ENDPOINT = BASE_URL + "/api/order/facts";
  private static final String TOKEN_ENDPOINT = BASE_URL + "/sessions/resume";
  private static final ObjectMapper MAPPER = new ObjectMapper();

  // Get user token from Pythia server and set it here for the client
  private static final String USER_TOKEN = "HIzwqZWKTqq50P08967U2Q";
  private static final String TABLE_NAME = "order";
  private static final List<String> FACT_FIELDS = List.of(
          "OrId",
          "Amount",
          "Status",
          "Message",
          "DispatchDate",
          "DeliveryDate"
  );

  private final Map<String, Instant> startTimes = new HashMap<>();
  private final Map<String, String> contexts = new HashMap<>();

  @Override
  public void beforeTestExecution(ExtensionContext context) {
    startTimes.put(context.getUniqueId(), Instant.now());
    contexts.put(context.getUniqueId(), context.getDisplayName() + Instant.now().toString());

    System.out.printf("[Before] Running test: %s at %s %n", context.getDisplayName(), context.getUniqueId());
  }

  @Override
  public void afterTestExecution(ExtensionContext context) {
    System.out.printf("[After] Finished test: %s%n", context.getDisplayName());

    try {
      var db = (Database) invoke(context.getRequiredTestInstance(), "mainDb");
      List<Map<String, String>> facts = fetchChangedFacts(db, context);
      sendFactsToServer(facts);
    } catch (Exception e) {
      throw new RuntimeException("Error in afterTestExecution", e);
    }
  }

  private Object invoke(Object instance, String methodName) {
    Class<?> clazz = instance.getClass();
    while (clazz != null) {
      try {
        Method m = clazz.getDeclaredMethod(methodName);
        m.setAccessible(true);
        return m.invoke(instance);
      } catch (NoSuchMethodException e) {
        clazz = clazz.getSuperclass();
      } catch (Exception e) {
        throw new RuntimeException("Failed to invoke method: " + methodName, e);
      }
    }
    return null;
  }

  private List<Map<String, String>> fetchChangedFacts(Database db, ExtensionContext context) {
    return db
            .sqlQuery(
                    buildFactsQuery(TABLE_NAME, FACT_FIELDS)
            )
            .setParameter("Context", contexts.get(context.getUniqueId()))
            .setParameter("Time0", startTimes.get(context.getUniqueId()))
            .findList()
            .stream()
            .map(row -> {
              Map<String, String> map = new LinkedHashMap<>();
              FACT_FIELDS.forEach(column -> {
                var value = row.get(column);
                map.put(column, value != null ? value.toString() : null);
              });
              map.put("Context", row.get("Context").toString());
              map.put("EditTime1", row.get("EditTime1").toString());
              map.put("EditTime2", row.get("EditTime2").toString());
              map.put("SeqNum", row.get("SeqNum").toString());
              return map;
            })
            .toList();
  }

  private static String buildFactsQuery(String tableName, List<String> fields) {
    String fieldList = fields.stream()
            .map(f -> String.format("%s.%s AS \"%s\"", tableName, f, f))
            .reduce((a, b) -> a + ",\n          " + b)
            .orElse("");

    String extraColumns = """
                  :Context AS "Context",
                  %1$s.EditTime AS "EditTime1",
                  %1$s.EditTime AS "EditTime2"
            """.formatted(tableName);

    String selectMain = """
                SELECT
                  %1$s
                  ,CAST(%2$s.SeqNum AS VARCHAR) AS "SeqNum"
                FROM %2$s
                WHERE %2$s.EditTime > :Time0
            """.formatted(fieldList + ",\n" + extraColumns, tableName);

    String auditTableName = "Audit_" + tableName;

    fieldList = fields.stream()
            .map(f -> String.format("%s.%s AS \"%s\"", auditTableName, f, f))
            .reduce((a, b) -> a + ",\n          " + b)
            .orElse("");

    extraColumns = """
                  :Context AS "Context",
                  %1$s.EditTime AS "EditTime1",
                  %1$s.EditTime AS "EditTime2"
            """.formatted(auditTableName);

    String selectAudit = """
                SELECT
                  %1$s
                  ,CAST(%2$s.SeqNum AS VARCHAR) AS "SeqNum"
                FROM %2$s
                WHERE %2$s.EditTime > :Time0
            """.formatted(fieldList + ",\n" + extraColumns, auditTableName);

    return selectMain + "\nUNION\n" + selectAudit;
  }

  private void sendFactsToServer(List<Map<String, String>> facts) throws Exception {
    Map<String, Object> payload = Map.of("facts", facts);
    String json = MAPPER.writeValueAsString(payload);

    HttpClient client = createAuthenticatedHttpClient();
    HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create(FACTS_ENDPOINT))
            .header("Content-Type", "application/json")
            .POST(HttpRequest.BodyPublishers.ofString(json))
            .build();

    HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

    System.out.printf("POST %s -> %d %s%n", FACTS_ENDPOINT, response.statusCode(), response.body());
  }

  private HttpClient createAuthenticatedHttpClient() throws Exception {
    CookieManager cookieManager = new CookieManager();
    HttpClient client = HttpClient.newBuilder()
            .cookieHandler(cookieManager)
            .build();

    HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create(TOKEN_ENDPOINT))
            .header("Content-Type", "application/x-www-form-urlencoded")
            .POST(HttpRequest.BodyPublishers.ofString(
                    "user_token=" + URLEncoder.encode(USER_TOKEN, StandardCharsets.UTF_8)))
            .build();

    HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
    System.out.printf("Auth response: %d %s%n", response.statusCode(), response.body());

    return client;
  }
}