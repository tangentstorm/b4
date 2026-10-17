use std::io::BufRead;

fn main() {
    let mut b4 = b4::B4::new(Box::new(std::io::stdout()));
    b4.b4h_mode = true;
    std::process::exit(b4::b4i::run(&mut b4, std::io::stdin().lock().lines()));
}
