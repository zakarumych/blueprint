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
    A,
    B(u8, String),
    C { a: Vec<u8>, b: f32 },
}

fn main() {
    println!("{}", Foo::BLUEPRINT);
    println!("{}", Bar::BLUEPRINT);
    println!("{}", Baz::BLUEPRINT);
    println!("{}", Eee::BLUEPRINT);
}
