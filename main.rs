// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
  executable: String,
  args: Vec<String>,
  env: Vec<String>,
  current_dir: String,
}
// impl CommandBuilder {
//   fn build(&self) -> std::result::Result<Command, Box<dyn std::error::Error>> {
//     Ok(Command {
//       executable: self.executable.clone().unwrap(),
//       args: self.args.clone().unwrap(),
//       env: self.env.clone().unwrap(),
//       current_dir: self.current_dir.clone().unwrap(),
//     })
//   }
// }
fn main() {}
