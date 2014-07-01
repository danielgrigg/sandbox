fn match_number(x:int) {
    match x {
    0       => println("zero"),
    1 | 2   => println("one or two"), 
    3..10   => println("3 to 10"), 
    _       => println("something else")
  }
}

fn main() {
  let my_number = 5;
  match_number(my_number);
  match_number(2);
  match_number(19);
}

