use std::{env, fs, path::Path};

fn main() {
    let json_data =
        fs::read_to_string(Path::new("data/types.json")).expect("Failed to read JSON file");
    let objects: Vec<serde_json::Value> =
        serde_json::from_str(&json_data).expect("Invalid JSON format");

    let mut output = String::new();

    let file_path = Path::new("data").join("changes.pl");
    let contents = fs::read_to_string(&file_path)
        .unwrap_or_else(|_| panic!("Could not find file: {:?}", file_path));
    output.push_str("% changes.pl\n"); // optional separator
    output.push_str(&contents);
    output.push_str("\n\n"); // optional separator

    let file_path = Path::new("data").join("record.pl");
    let contents = fs::read_to_string(&file_path)
        .unwrap_or_else(|_| panic!("Could not find file: {:?}", file_path));
    output.push_str("% record.pl\n"); // optional separator
    output.push_str(&contents);
    output.push_str("\n\n"); // optional separator

    for o in objects {
        if let Some(name) = o.get("name").and_then(|v| v.as_str()) {
            let file_path = Path::new("data").join(format!("{}.pl", name));
            let contents = fs::read_to_string(&file_path)
                .unwrap_or_else(|_| panic!("Could not find file: {:?}", file_path));
            output.push_str(format!("% {}.pl\n", name).as_str()); // optional separator
            output.push_str(&contents);
            output.push_str("\n\n"); // optional separator
        }
    }

    // Write to output file inside OUT_DIR
    let out_dir = env::var("OUT_DIR").unwrap();
    let out_path = Path::new(&out_dir).join("pythia.pl");
    println!("cargo:warning=output path: {:?}", out_path);
    fs::write(&out_path, output).expect("Failed to write output file");

    //  fs::write("src/generated.rs", output).expect("Failed to write generated.rs");
    // Tell Cargo to rerun this build script if these files change
    println!("cargo:rerun-if-changed=data/types.json");
    println!("cargo:rerun-if-changed=data/");
}

