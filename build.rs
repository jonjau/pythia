use std::{env, fs, io, path::Path, process::id};

fn main() {
    let json_data =
        fs::read_to_string(Path::new("data/types.json")).expect("Failed to read JSON file");
    let objects: Vec<serde_json::Value> =
        serde_json::from_str(&json_data).expect("Invalid JSON format");

    let imports = objects
        .iter()
        .map(|o| {
            o.get("name")
                .and_then(|v| v.as_str())
                .and_then(|name| Some(format!(":- use_module('./data/{}.pl').", name)))
        })
        .flatten()
        .collect::<Vec<_>>()
        .join("\n");

    let record_predicates = objects
        .iter()
        .map(|o| {
            let name = o.get("name").and_then(|v| v.as_str());

            let id_fields = o
                .get("id_fields")
                .and_then(|vs| vs.as_array())
                .and_then(|vs| Some(vs.iter().map(|v| v.as_str()).flatten().collect::<Vec<_>>()));

            let data_fields = o
                .get("data_fields")
                .and_then(|vs| vs.as_array())
                .and_then(|vs| Some(vs.iter().map(|v| v.as_str()).flatten().collect::<Vec<_>>()));

            let metadata_fields = o
                .get("metadata_fields")
                .and_then(|vs| vs.as_array())
                .and_then(|vs| Some(vs.iter().map(|v| v.as_str()).flatten().collect::<Vec<_>>()));

            if let (Some(name), Some(id_fields), Some(data_fields), Some(metadata_fields)) =
                (name, id_fields, data_fields, metadata_fields)
            {
                if id_fields.is_empty() || metadata_fields.is_empty() {
                    None
                } else {
                    Some(format!(
                        "record({}, {}, [{}]) :-\n    {}({}, {}, {}).",
                        metadata_fields.join(", "),
                        id_fields.join(", "),
                        data_fields.join(", "),
                        name,
                        id_fields.join(", "),
                        data_fields.join(", "),
                        metadata_fields.join(", ")
                    ))
                }
            } else {
                None
            }
        })
        .flatten()
        .collect::<Vec<_>>()
        .join("\n\n");

    println!("cargo:warning={:?}", record_predicates);

    let start_marker = "% BEGIN fact-imports";
    let end_marker = "% END fact-imports";
    let _ = replace_section(
        "data/internal/pythia.pl",
        &imports,
        start_marker,
        end_marker,
    );

    let start_marker = "% BEGIN record-predicates";
    let end_marker = "% END record-predicates";
    let _ = replace_section(
        "data/internal/pythia.pl",
        &record_predicates,
        start_marker,
        end_marker,
    );

    // Tell Cargo to rerun this build script if these files change
    println!("cargo:rerun-if-changed=data/types.json");
    println!("cargo:rerun-if-changed=data/");
}

fn replace_section(
    filepath: &str,
    new_content: &str,
    start_marker: &str,
    end_marker: &str,
) -> io::Result<()> {
    let contents = fs::read_to_string(filepath)?;

    // Find the start and end positions
    if let (Some(start_pos), Some(end_pos)) =
        (contents.find(start_marker), contents.find(end_marker))
    {
        let start_pos = start_pos + start_marker.len(); // Move past the start marker
        let before = &contents[..start_pos];
        let after = &contents[end_pos..];

        // Build the new file contents
        let mut new_file_contents = String::new();
        new_file_contents.push_str(before);
        new_file_contents.push_str("\n\n");
        new_file_contents.push_str(new_content);
        new_file_contents.push_str("\n\n");
        new_file_contents.push_str(after);

        fs::write(filepath, new_file_contents)?;
    } else {
        // Either marker is missing
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Markers not found",
        ));
    }

    Ok(())
}
