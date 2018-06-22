extern crate disa;

use disa::AvrInsn;


fn main() {
    let insn = AvrInsn::decode(0x0173);

    println!("{:?}", insn);
}
