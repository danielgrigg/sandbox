fn is_four(x: int) -> bool {
  x == 4
}

fn main() {
  let item = "muffin";

  let price = 
    if item == "salad" {
      3.5
    } else if item == "muffin" {
      2.25
    } else {
      2.00
    };
  println(fmt!("item %? costs $%?", item, price));

  println(fmt!("x(3) == 4? : %?", is_four(3)));
}

