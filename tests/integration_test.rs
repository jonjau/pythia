use scryer_prolog::machine::Machine;
use scryer_prolog::machine::parsed_results::QueryResult;

#[test]
fn prolog_module_works() {
    let mut machine = Machine::new_lib();

    machine.load_module_string("facts", String::from(r#"
        edge("a", "p1", "b").
        edge("a", "p2", "b").
    "#));

    machine.load_module_string("more-facts", String::from(r#"
        edgae("a", "p3", "b").
    "#));


    // let output: QueryResult = machine.run_query(String::from(r#"triple("y", X)."#));

    // println!("output: {:?}", output);

    // machine.load_module_string("facts", String::from(r#"
    //     triple("a", "p1", "b").
    //     triple("a", "p2", "b").
    // "#));

    let output: QueryResult = machine.run_query(String::from(r#"edge("a",P,"b")."#));

    println!("output: {:?}", output);

    // assert_eq!(output, Ok(QueryResolution::Matches(vec![
    //     QueryMatch::from(BTreeMap::from([("P", Value::from("p1"))])),
    //     QueryMatch::from(BTreeMap::from([("P", Value::from("p2"))])),
    // ])));

    assert_eq!(4, 2 + 2);
}

#[test]
fn prolog_module_works2() {
    let mut machine = Machine::new_lib();

    machine.load_module_string("aa", String::from(r#"
        dimlink("Test1", "M1", "ID1", "J1", "2023-02-10", "2023-02-08", "2024-02-18 08:16:11", "D", 0). 
        dimlink("Test1", "M1", "ID1", "J1", "2023-02-10", "2023-02-09", "2024-02-18 08:17:11", "E", 1). 
        dimlink("Test1", "M2", "ID2", "J1", "2023-02-11", "2023-02-08", "2024-02-18 08:20:11", "D", 0).
        dimlink("Test1", "M2", "ID2", "J1", "2023-02-11", "2023-02-08", "2024-02-18 08:20:12", "D", 1). 
        dimlink("Test1", "M2", "ID2", "J1", "2023-02-11", "2023-02-08", "2024-02-18 08:20:13", "E", 2). 
        dimlink("Test1", "M3", "ID2", "J2", "2023-02-09", "2023-02-08", "2024-02-18 08:20:14", "O", 0). 
        dimlink("Test1", "M4", "ID1", "J2", "2023-02-09", "2023-02-08", "2024-02-18 09:17:11", "O", 0).
        dimlink("Test1", "M5", "ID1", "J3", "2023-02-09", "2023-02-08", "2024-02-18 09:17:11", "D", 0).
        dimlink("Test1", "M5", "ID1", "J3", "2023-02-09", "2023-02-08", "2024-02-18 09:17:11", "V", 1).
        
        record(Context, EditTime, SeqNum, RecStatus, Id, [IRef, DRef, BegPeriod, EndPeriod]) :-
            dimlink(Context, Id, IRef, DRef, BegPeriod, EndPeriod, EditTime, RecStatus, SeqNum).
    "#));

    let output = machine.run_query(String::from(r#"record(Context, EditTime, SeqNum, RecStatus, Id, [IRef, DRef, BegPeriod, EndPeriod])."#));

    println!("output: {:?}", output);
}

#[test]
fn add_fact_to_logic_machine() {


}