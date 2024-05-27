use std::rc::Rc;

struct Todo {
    title: TodoTitle,
    description: TodoDescription,
    date: Date,
    time: Time,
    duration: Duration,
    recurrence: Recurrence,
    tags: Rc<[Tag]>,
}

struct TodoTitle(Rc<str>);

struct TodoDescription(Rc<str>);
struct Date();
struct Time();
struct Duration();
struct Recurrence();
struct Deadline();
struct Tag();


fn main() {
    println!("Hello, world!");
}
