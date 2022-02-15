use blueprint::Blueprinted;

#[derive(blueprint::Blueprinted)]
struct Foo;

#[derive(blueprint::Blueprinted)]
struct Bar {
    val: u32,
}

#[derive(blueprint::Blueprinted)]
struct Baz(String, [u8; 42]);

#[derive(blueprint::Blueprinted)]
enum Eee {
    Unit,
    Tuple(u8, String),
    Struct { a: Vec<u8>, b: f32 },
}

fn main() {
    println!("{}", &serde_json::to_string_pretty(Foo::BLUEPRINT).unwrap());
    println!("{}", &serde_json::to_string_pretty(Bar::BLUEPRINT).unwrap());
    println!("{}", &serde_json::to_string_pretty(Baz::BLUEPRINT).unwrap());
    println!("{}", &serde_json::to_string_pretty(Eee::BLUEPRINT).unwrap());
}
