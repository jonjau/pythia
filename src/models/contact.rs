use std::collections::HashSet;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Contact {
    pub id: String,
    pub first: String,
    pub last: String,
    pub email: String,
    pub phone: String,
}

#[derive(Clone)]
pub struct ContactModel {
    all_contacts: HashSet<Contact>,
}

impl ContactModel {
    pub fn new() -> ContactModel {
        ContactModel {
            all_contacts: vec![
                Contact {
                    id: String::from("1"),
                    first: String::from("John"),
                    last: String::from("Doe"),
                    email: String::from("jdoe@gmail.com"),
                    phone: String::from("123"),
                },
                Contact {
                    id: String::from("2"),
                    first: String::from("Jane"),
                    last: String::from("Doe"),
                    email: String::from("jane@gmail.com"),
                    phone: String::from("321"),
                },
            ]
            .into_iter()
            .collect(),
        }
    }

    pub fn find(&self, search_str: String) -> HashSet<Contact> {
        self.all_contacts
            .iter()
            .filter(|c| (c.first.clone() + " " + &c.last).contains(&search_str))
            .cloned()
            .collect()
    }

    pub fn find_all(&self) -> HashSet<Contact> {
        self.all_contacts.clone()
    }
}
