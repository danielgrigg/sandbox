fn mymap<T, U>(vector: &[T], function: |v: &T| -> U) -> ~[U] {
  let mut acc = ~[];
  for element in vector.iter() {
    acc.push(function(element));
  }
  return acc;
}

fn main() {
  let mut max = 0;
  [1,2,3].map(|x| if *x > max { max = *x });

  println(fmt!("max = %?", max));
}
