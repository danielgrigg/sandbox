fn main() {
  struct Foo { x: int, y: ~int }

  {
    let a = Foo { x: 5, y: ~10 };
  }

  let mut b = Foo { x: 5, y: ~10 };
  b.x = 10;

  println(fmt!("b.x: %?", b));

}
