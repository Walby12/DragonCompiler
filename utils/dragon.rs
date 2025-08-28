fn main() {
    let mut x: i64 = 0;
    while (x < 3) {
println!("{}", x);
x = (x + 1);
}

    if (x == 3) {
println!("{}", String::from("done!"));
}
else {
println!("{}", String::from("not done?"));
}

}

