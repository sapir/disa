#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Reg(pub u8);

pub const X_L: Reg = Reg(26);
pub const X_H: Reg = Reg(27);
pub const Y_L: Reg = Reg(28);
pub const Y_H: Reg = Reg(29);
pub const Z_L: Reg = Reg(30);
pub const Z_H: Reg = Reg(31);


#[derive(Debug, Copy, Clone, PartialEq)]
pub struct RegPair(pub u8);

impl RegPair {
    pub fn low(&self) -> Reg {
        match *self {
            RegPair(r) => Reg(r),
        }
    }

    pub fn high(&self) -> Reg {
        match *self {
            RegPair(r) => Reg(r + 1),
        }
    }
}

pub const X: RegPair = RegPair(X_L.0);
pub const Y: RegPair = RegPair(Y_L.0);
pub const Z: RegPair = RegPair(Z_L.0);


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MemRegUpdate {
    None,
    PostInc,
    PreDec,
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub struct MemAccess {
    pub reg_pair: RegPair,
    pub ofs: u8,
    pub update: MemRegUpdate,
}

impl MemAccess {
    pub fn just(reg_pair: RegPair) -> MemAccess {
        MemAccess { reg_pair, ofs: 0, update: MemRegUpdate::None }
    }

    pub fn post_inc(reg_pair: RegPair) -> MemAccess {
        MemAccess { reg_pair, ofs: 0, update: MemRegUpdate::PostInc }
    }

    pub fn pre_dec(reg_pair: RegPair) -> MemAccess {
        MemAccess { reg_pair, ofs: 0, update: MemRegUpdate::PreDec }
    }

    pub fn with_ofs(reg_pair: RegPair, ofs: u8) -> MemAccess {
        MemAccess { reg_pair, ofs, update: MemRegUpdate::None }
    }
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AvrInsn {
    Nop,
    Movw(RegPair, RegPair),
    Muls(Reg, Reg),
    Mulsu(Reg, Reg),
    Fmul(Reg, Reg),
    Fmuls(Reg, Reg),
    Fmulsu(Reg, Reg),

    Cpc(Reg, Reg),
    Cp(Reg, Reg),
    Sbc(Reg, Reg),
    Sub(Reg, Reg),
    Add(Reg, Reg),
    Adc(Reg, Reg),
    Cpse(Reg, Reg),
    And(Reg, Reg),
    Eor(Reg, Reg),
    Or(Reg, Reg),
    Mov(Reg, Reg),

    Cpi(Reg, u8),

    Sbci(Reg, u8),
    Subi(Reg, u8),
    Ori(Reg, u8),
    Sbr,
    Andi(Reg, u8),
    Cbr,

    Ldd(Reg, MemAccess),
    Std(MemAccess, Reg),

    // lds/sts without the extra immediate word
    LdsWord1(Reg),
    StsWord1(Reg),
    Lds(Reg, u16),
    Sts(u16, Reg),

    Xch(MemAccess, Reg),
    Las(MemAccess, Reg),
    Lac(MemAccess, Reg),
    Lat(MemAccess, Reg),

    Ld(Reg, MemAccess),
    St(MemAccess, Reg),
    LpmZ(Reg, MemAccess),
    ElpmZ(Reg, MemAccess),

    Pop(Reg),
    Push(Reg),

    Com(Reg),
    Neg(Reg),
    Swap(Reg),
    Inc(Reg),

    Asr(Reg),
    Lsr(Reg),
    Ror(Reg),

    Sec,
    Sez,
    Sen,
    Sev,
    Ses,
    Seh,
    Set,
    Sei,
    Clc,
    Clz,
    Cln,
    Clv,
    Cls,
    Clh,
    Clt,
    Cli,

    Ret,
    Reti,

    Sleep,
    Break,
    Wdr,

    Lpm,
    Elpm,
    Spm,
    SpmZ(MemAccess),

    Ijmp,
    Icall,
    Eijmp,
    Eicall,

    Dec(Reg),

    Des(u8),

    // jmp/call without the extra immediate word
    JmpWord1(u8),
    CallWord1(u8),

    Jmp(u32),
    Call(u32),

    Adiw(RegPair, u8),
    Sbiw(RegPair, u8),

    Cbi(u8, u8),
    Sbi(u8, u8),
    Sbic(u8, u8),
    Sbis(u8, u8),

    Mul(Reg, Reg),

    In(Reg, u8),
    Out(u8, Reg),

    Rjmp(i16),
    Rcall(i16),

    Ldi(Reg, u8),

    Brcc(i8),
    Brcs(i8),
    Brne(i8),
    Breq(i8),
    Brpl(i8),
    Brmi(i8),
    Brvc(i8),
    Brvs(i8),
    Brge(i8),
    Brlt(i8),
    Brhc(i8),
    Brhs(i8),
    Brtc(i8),
    Brts(i8),
    Brid(i8),
    Brie(i8),

    Bld(Reg, u8),
    Bst(Reg, u8),

    Sbrc(Reg, u8),
    Sbrs(Reg, u8),
}


fn extract_bits(word: u16, hi: u8, lo: u8) -> u16 {
    assert!(hi >= lo);
    (word >> lo) & ((1 << (hi - lo + 1)) - 1)
}

fn get_bit(word: u16, bit: u8) -> u8 {
    extract_bits(word, bit, bit) as u8
}


impl AvrInsn {
    pub fn decode_word(word: u16) -> Option<AvrInsn> {
        let bits = |hi: u8, lo: u8| -> u16 {
            extract_bits(word, hi, lo)
        };

        let bit = |b: u8| -> u8 {
            get_bit(word, b)
        };

        let d5 = bits(8, 4) as u8;
        let d4 = bits(7, 4) as u8;
        let d3 = bits(6, 4) as u8;
        let r4 = bits(3, 0) as u8;
        let split_r5 = (bit(9) << 4) | r4;
        let r3 = bits(2, 0) as u8;
        let a5 = bits(7, 3) as u8;
        let b3 = bits(2, 0) as u8;

        let rd5 = Reg(d5);
        let rd4 = Reg(d4 + 16);
        let rd3 = Reg(d3 + 16);
        let rr5 = Reg(split_r5);
        let rr4 = Reg(r4 + 16);
        let rr3 = Reg(r3 + 16);

        let split_k8 = ((bits(11, 8) << 4) | bits(3, 0)) as u8;
        let split_a6 = ((bits(10, 9) << 4) | bits(3, 0)) as u8;

        let all_bits = (
            bit(15), bit(14), bit(13), bit(12),
            bit(11), bit(10), bit(9), bit(8),
            bit(7), bit(6), bit(5), bit(4),
            bit(3), bit(2), bit(1), bit(0),
        );

        match all_bits {
            (0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0) =>
                Some(AvrInsn::Nop),

            (0,0,0,0, 0,0,0,1, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Movw(RegPair(d4 << 1), RegPair(r4 << 1))),

            (0,0,0,0, 0,0,1,0, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Muls(rd4, rr4)),

            (0,0,0,0, 0,0,1,1, 0,_,_,_, 0,_,_,_) =>
                Some(AvrInsn::Mulsu(rd3, rr3)),

            (0,0,0,0, 0,0,1,1, 0,_,_,_, 1,_,_,_) =>
                Some(AvrInsn::Fmul(rd3, rr3)),

            (0,0,0,0, 0,0,1,1, 1,_,_,_, 0,_,_,_) =>
                Some(AvrInsn::Fmuls(rd3, rr3)),

            (0,0,0,0, 0,0,1,1, 1,_,_,_, 1,_,_,_) =>
                Some(AvrInsn::Fmulsu(rd3, rr3)),

            (0,0,0,0, 0,1,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Cpc(rd5, rr5)),

            (0,0,0,1, 0,1,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Cp(rd5, rr5)),

            (0,0,0,0, 1,0,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Sbc(rd5, rr5)),

            (0,0,0,1, 1,0,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Sub(rd5, rr5)),

            (0,0,0,0, 1,1,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Add(rd5, rr5)),

            (0,0,0,1, 1,1,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Adc(rd5, rr5)),

            (0,0,0,1, 0,0,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Cpse(rd5, rr5)),

            (0,0,1,0, 0,0,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::And(rd5, rr5)),

            (0,0,1,0, 0,1,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Eor(rd5, rr5)),

            (0,0,1,0, 1,0,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Or(rd5, rr5)),

            (0,0,1,0, 1,1,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Mov(rd5, rr5)),

            (0,0,1,1, _,_,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Cpi(rd4, split_k8)),
            (0,1,0,0, _,_,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Sbci(rd4, split_k8)),
            (0,1,0,1, _,_,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Subi(rd4, split_k8)),
            // TODO: ori=sbr?
            (0,1,1,0, _,_,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Ori(rd4, split_k8)),
            // TODO: andi=cbr?
            (0,1,1,1, _,_,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Andi(rd4, split_k8)),

            // ldd/std Z/Y+k
            (1,0,_,0, _,_,_,_, _,_,_,_, _,_,_,_) => {
                let ldd_std_k =
                    (bit(13) << 5)
                    | ((bits(11, 10) << 3) as u8)
                    | (bits(2, 0) as u8);

                let reg_pair =
                    if bit(3) == 1 {
                        Y
                    } else {
                        Z
                    };

                if bit(9) == 1 {
                    let opcode =
                        if ldd_std_k == 0 {
                            AvrInsn::St
                        } else {
                            AvrInsn::Std
                        };

                    Some(opcode(
                        MemAccess::with_ofs(reg_pair, ldd_std_k),
                        rd5))
                } else {
                    let opcode =
                        if ldd_std_k == 0 {
                            AvrInsn::Ld
                        } else {
                            AvrInsn::Ldd
                        };

                    Some(opcode(
                        rd5,
                        MemAccess::with_ofs(reg_pair, ldd_std_k)))
                }
            },

            (1,0,0,1, 0,0,0,_, _,_,_,_, 0,0,0,0) =>
                Some(AvrInsn::LdsWord1(rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 0,0,0,0) =>
                Some(AvrInsn::StsWord1(rd5)),

            (1,0,0,1, 0,0,0,_, _,_,_,_, 0,0,0,1) =>
                Some(AvrInsn::Ld(rd5, MemAccess::post_inc(Z))),
            (1,0,0,1, 0,0,0,_, _,_,_,_, 1,0,0,1) =>
                Some(AvrInsn::Ld(rd5, MemAccess::post_inc(Y))),
            (1,0,0,1, 0,0,0,_, _,_,_,_, 0,0,1,0) =>
                Some(AvrInsn::Ld(rd5, MemAccess::pre_dec(Z))),
            (1,0,0,1, 0,0,0,_, _,_,_,_, 1,0,1,0) =>
                Some(AvrInsn::Ld(rd5, MemAccess::pre_dec(Y))),

            (1,0,0,1, 0,0,1,_, _,_,_,_, 0,0,0,1) =>
                Some(AvrInsn::St(MemAccess::post_inc(Z), rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 1,0,0,1) =>
                Some(AvrInsn::St(MemAccess::post_inc(Y), rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 0,0,1,0) =>
                Some(AvrInsn::St(MemAccess::pre_dec(Z), rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 1,0,1,0) =>
                Some(AvrInsn::St(MemAccess::pre_dec(Y), rd5)),

            (1,0,0,1, 0,0,0,_, _,_,_,_, 0,1,_,_) => {
                let opcode =
                    if bit(1) == 1 {
                        AvrInsn::ElpmZ
                    } else {
                        AvrInsn::LpmZ
                    };

                let mem_operand =
                    if bit(0) == 1 {
                        MemAccess::post_inc(Z)
                    } else {
                        MemAccess::just(Z)
                    };

                Some(opcode(rd5, mem_operand))
            },

            (1,0,0,1, 0,0,1,_, _,_,_,_, 0,1,0,0) =>
                Some(AvrInsn::Xch(MemAccess::just(Z), rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 0,1,0,1) =>
                Some(AvrInsn::Las(MemAccess::just(Z), rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 0,1,1,0) =>
                Some(AvrInsn::Lac(MemAccess::just(Z), rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 0,1,1,1) =>
                Some(AvrInsn::Lat(MemAccess::just(Z), rd5)),

            (1,0,0,1, 0,0,0,_, _,_,_,_, 1,1,0,0) =>
                Some(AvrInsn::Ld(rd5, MemAccess::just(X))),
            (1,0,0,1, 0,0,0,_, _,_,_,_, 1,1,0,1) =>
                Some(AvrInsn::Ld(rd5, MemAccess::post_inc(X))),
            (1,0,0,1, 0,0,0,_, _,_,_,_, 1,1,1,0) =>
                Some(AvrInsn::Ld(rd5, MemAccess::pre_dec(X))),

            (1,0,0,1, 0,0,1,_, _,_,_,_, 1,1,0,0) =>
                Some(AvrInsn::St(MemAccess::just(X), rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 1,1,0,1) =>
                Some(AvrInsn::St(MemAccess::post_inc(X), rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 1,1,1,0) =>
                Some(AvrInsn::St(MemAccess::pre_dec(X), rd5)),

            (1,0,0,1, 0,0,0,_, _,_,_,_, 1,1,1,1) =>
                Some(AvrInsn::Pop(rd5)),
            (1,0,0,1, 0,0,1,_, _,_,_,_, 1,1,1,1) =>
                Some(AvrInsn::Push(rd5)),

            (1,0,0,1, 0,1,0,_, _,_,_,_, 0,0,0,0) =>
                Some(AvrInsn::Com(rd5)),
            (1,0,0,1, 0,1,0,_, _,_,_,_, 0,0,0,1) =>
                Some(AvrInsn::Neg(rd5)),
            (1,0,0,1, 0,1,0,_, _,_,_,_, 0,0,1,0) =>
                Some(AvrInsn::Swap(rd5)),
            (1,0,0,1, 0,1,0,_, _,_,_,_, 0,0,1,1) =>
                Some(AvrInsn::Inc(rd5)),
            (1,0,0,1, 0,1,0,_, _,_,_,_, 0,1,0,1) =>
                Some(AvrInsn::Asr(rd5)),
            (1,0,0,1, 0,1,0,_, _,_,_,_, 0,1,1,0) =>
                Some(AvrInsn::Lsr(rd5)),
            (1,0,0,1, 0,1,0,_, _,_,_,_, 0,1,1,1) =>
                Some(AvrInsn::Ror(rd5)),

            (1,0,0,1, 0,1,0,0, 0,0,0,0, 1,0,0,0) =>
                Some(AvrInsn::Sec),
            (1,0,0,1, 0,1,0,0, 0,0,0,1, 1,0,0,0) =>
                Some(AvrInsn::Sez),
            (1,0,0,1, 0,1,0,0, 0,0,1,0, 1,0,0,0) =>
                Some(AvrInsn::Sen),
            (1,0,0,1, 0,1,0,0, 0,0,1,1, 1,0,0,0) =>
                Some(AvrInsn::Sev),
            (1,0,0,1, 0,1,0,0, 0,1,0,0, 1,0,0,0) =>
                Some(AvrInsn::Ses),
            (1,0,0,1, 0,1,0,0, 0,1,0,1, 1,0,0,0) =>
                Some(AvrInsn::Seh),
            (1,0,0,1, 0,1,0,0, 0,1,1,0, 1,0,0,0) =>
                Some(AvrInsn::Set),
            (1,0,0,1, 0,1,0,0, 0,1,1,1, 1,0,0,0) =>
                Some(AvrInsn::Sei),
            (1,0,0,1, 0,1,0,0, 1,0,0,0, 1,0,0,0) =>
                Some(AvrInsn::Clc),
            (1,0,0,1, 0,1,0,0, 1,0,0,1, 1,0,0,0) =>
                Some(AvrInsn::Clz),
            (1,0,0,1, 0,1,0,0, 1,0,1,0, 1,0,0,0) =>
                Some(AvrInsn::Cln),
            (1,0,0,1, 0,1,0,0, 1,0,1,1, 1,0,0,0) =>
                Some(AvrInsn::Clv),
            (1,0,0,1, 0,1,0,0, 1,1,0,0, 1,0,0,0) =>
                Some(AvrInsn::Cls),
            (1,0,0,1, 0,1,0,0, 1,1,0,1, 1,0,0,0) =>
                Some(AvrInsn::Clh),
            (1,0,0,1, 0,1,0,0, 1,1,1,0, 1,0,0,0) =>
                Some(AvrInsn::Clt),
            (1,0,0,1, 0,1,0,0, 1,1,1,1, 1,0,0,0) =>
                Some(AvrInsn::Cli),

            (1,0,0,1, 0,1,0,1, 0,0,0,0, 1,0,0,0) =>
                Some(AvrInsn::Ret),
            (1,0,0,1, 0,1,0,1, 0,0,0,1, 1,0,0,0) =>
                Some(AvrInsn::Reti),

            (1,0,0,1, 0,1,0,1, 1,0,0,0, 1,0,0,0) =>
                Some(AvrInsn::Sleep),
            (1,0,0,1, 0,1,0,1, 1,0,0,1, 1,0,0,0) =>
                Some(AvrInsn::Break),
            (1,0,0,1, 0,1,0,1, 1,0,1,0, 1,0,0,0) =>
                Some(AvrInsn::Wdr),

            (1,0,0,1, 0,1,0,1, 1,1,0,0, 1,0,0,0) =>
                Some(AvrInsn::Lpm),
            (1,0,0,1, 0,1,0,1, 1,1,0,1, 1,0,0,0) =>
                Some(AvrInsn::Elpm),

            (1,0,0,1, 0,1,0,1, 1,1,1,0, 1,0,0,0) =>
                Some(AvrInsn::Spm),  // TODO: just(Z)?
            (1,0,0,1, 0,1,0,1, 1,1,1,1, 1,0,0,0) =>
                Some(AvrInsn::SpmZ(MemAccess::post_inc(Z))),

            (1,0,0,1, 0,1,0,0, 0,0,0,0, 1,0,0,1) =>
                Some(AvrInsn::Ijmp),
            (1,0,0,1, 0,1,0,1, 0,0,0,0, 1,0,0,1) =>
                Some(AvrInsn::Icall),
            (1,0,0,1, 0,1,0,0, 0,0,0,1, 1,0,0,1) =>
                Some(AvrInsn::Eijmp),
            (1,0,0,1, 0,1,0,1, 0,0,0,1, 1,0,0,1) =>
                Some(AvrInsn::Eicall),

            (1,0,0,1, 0,1,0,_, _,_,_,_, 1,0,1,0) =>
                Some(AvrInsn::Dec(rd5)),

            (1,0,0,1, 0,1,0,0, _,_,_,_, 1,0,1,1) =>
                // round number is in same position at d4
                Some(AvrInsn::Des(d4)),

            (1,0,0,1, 0,1,0,_, _,_,_,_, 1,1,_,_) => {
                let opcode =
                    if bit(1) == 1 {
                        AvrInsn::CallWord1
                    } else {
                        AvrInsn::JmpWord1
                    };

                let split_k = (d5 << 1) | bit(0);

                Some(opcode(split_k))
            }

            (1,0,0,1, 0,1,1,_, _,_,_,_, _,_,_,_) => {
                let rp = RegPair(24 + ((bits(5, 4) as u8) << 1));
                let uimm6 = ((bits(7, 6) << 4) | bits(3, 0)) as u8;
                let opcode =
                    if bit(8) == 0 {
                        AvrInsn::Adiw
                    } else {
                        AvrInsn::Sbiw
                    };

                Some(opcode(rp, uimm6))
            }

            (1,0,0,1, 1,0,0,0, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Cbi(a5, b3)),
            (1,0,0,1, 1,0,1,0, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Sbi(a5, b3)),
            (1,0,0,1, 1,0,0,1, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Sbic(a5, b3)),
            (1,0,0,1, 1,0,1,1, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Sbis(a5, b3)),

            (1,0,0,1, 1,1,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Mul(rd5, rr5)),

            (1,0,1,1, 0,_,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::In(rd5, split_a6)),
            (1,0,1,1, 1,_,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Out(split_a6, rd5)),

            (1,1,0,_,_, _,_,_, _,_,_,_, _,_,_,_) => {
                let opcode =
                    if bit(12) == 1 {
                        AvrInsn::Rcall
                    } else {
                        AvrInsn::Rjmp
                    };

                let mut ofs = bits(11, 0) as i16;
                if ofs & (1 << 11) != 0 {
                    ofs -= 1 << 12;
                }

                // *2 to get offset in bytes rather than words
                Some(opcode(ofs * 2))
            },

            (1,1,1,0, _,_,_,_, _,_,_,_, _,_,_,_) =>
                Some(AvrInsn::Ldi(rd4, split_k8)),

            (1,1,1,1,0, _,_,_, _,_,_,_, _,_,_,_) => {
                let opcode =
                    match (bit(10), bits(2, 0)) {
                        (0, 0b000) => AvrInsn::Brcs,
                        (0, 0b001) => AvrInsn::Breq,
                        (0, 0b010) => AvrInsn::Brmi,
                        (0, 0b011) => AvrInsn::Brvs,
                        (0, 0b100) => AvrInsn::Brlt,
                        (0, 0b101) => AvrInsn::Brhs,
                        (0, 0b110) => AvrInsn::Brts,
                        (0, 0b111) => AvrInsn::Brie,

                        (1, 0b000) => AvrInsn::Brcc,
                        (1, 0b001) => AvrInsn::Brne,
                        (1, 0b010) => AvrInsn::Brpl,
                        (1, 0b011) => AvrInsn::Brvc,
                        (1, 0b100) => AvrInsn::Brge,
                        (1, 0b101) => AvrInsn::Brhc,
                        (1, 0b110) => AvrInsn::Brtc,
                        (1, 0b111) => AvrInsn::Brid,

                        _ => unreachable!(),
                    };

                let mut ofs = bits(9, 3) as i8;
                // sign-extend 7-bit to 8-bit
                if ofs & (1 << 6) != 0 {
                    ofs |= -0x80;
                }

                // *2 to get offset in bytes rather than words
                Some(opcode(ofs * 2))
            },

            (1,1,1,1, 1,0,_,_, _,_,_,_, 0,_,_,_) => {
                // bit index is in same position as r3
                let bit_index = r3;

                let opcode =
                    if bit(9) == 1 {
                        AvrInsn::Bst
                    } else {
                        AvrInsn::Bld
                    };

                Some(opcode(rd5, bit_index))
            },

            (1,1,1,1, 1,1,_,_, _,_,_,_, 0,_,_,_) => {
                let opcode =
                    if bit(9) == 1 {
                        AvrInsn::Sbrs
                    } else {
                        AvrInsn::Sbrc
                    };

                // bit index is in same position as r3
                Some(opcode(rd5, r3))
            },

            _ => None,
        }
    }

    pub fn decode(input: &[u16]) -> Option<(&[u16], AvrInsn)> {
        if input.is_empty() {
            return None;
        }

        let word = input[0];
        let insn = AvrInsn::decode_word(word)?;

        let rest = &input[1..];

        match insn {
            AvrInsn::LdsWord1(_) | AvrInsn::StsWord1(_) => {
                let addr = rest[0];
                let rest = &rest[1..];

                let full_insn = match insn {
                    AvrInsn::LdsWord1(reg) => AvrInsn::Lds(reg, addr),
                    AvrInsn::StsWord1(reg) => AvrInsn::Sts(addr, reg),
                    _ => unreachable!(),
                };

                Some((rest, full_insn))
            },

            AvrInsn::JmpWord1(val1) | AvrInsn::CallWord1(val1) => {
                let val2 = rest[0];
                let rest = &rest[1..];

                let opcode = match insn {
                    AvrInsn::JmpWord1(_) => AvrInsn::Jmp,
                    AvrInsn::CallWord1(_) => AvrInsn::Call,
                    _ => unreachable!(),
                };

                let val1 = val1 as u32;
                let val2 = val2 as u32;
                let addr = (val1 << 16) | val2;

                // *2 to get address in bytes
                let full_insn = opcode(addr * 2);

                Some((rest, full_insn))
            },

            _ => Some((rest, insn))
        }
    }

    pub fn byte_size(&self) -> usize {
        match self {
            AvrInsn::Lds(_, _) | AvrInsn::Sts(_, _)
                | AvrInsn::Jmp(_) | AvrInsn::Call(_) => 4,

            _ => 2,
        }
    }

    pub fn get_rel_jmp_target(next_pc: u32, ofs: i16) -> u32 {
        next_pc.wrapping_add(ofs as i32 as u32)
    }
}
