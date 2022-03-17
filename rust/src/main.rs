use std::{
    io::{BufRead, Write},
    net::TcpStream,
};

const ENDPOINT: &'static str = "piecewise.challs.cyberchallenge.it:9110";

#[derive(Debug, Clone, Copy)]
pub enum Endianness {
    Big,
    Little,
}

#[derive(Debug, Clone, Copy)]
pub enum Number {
    U32(u32),
    U64(u64),
}

impl Number {
    pub fn to_be(self) -> Self {
        match self {
            Self::U32(n) => Self::U32(n.to_be()),
            Self::U64(n) => Self::U64(n.to_be()),
        }
    }

    pub fn to_le(self) -> Self {
        match self {
            Self::U32(n) => Self::U32(n.to_le()),
            Self::U64(n) => Self::U64(n.to_le()),
        }
    }

    pub fn convert(self, e: Endianness) -> Self {
        match e {
            Endianness::Big => self.to_be(),
            Endianness::Little => self.to_le(),
        }
    }

    pub fn into_vec(self) -> Vec<u8> {
        match self {
            Self::U32(n) => {
                let len = std::mem::size_of::<u32>();
                let mut res = Vec::with_capacity(len);
                unsafe { res.set_len(len) };
                unsafe { std::ptr::write_unaligned(res.as_mut_ptr() as *mut u32, n) };
                res
            }
            Self::U64(n) => {
                let len = std::mem::size_of::<u64>();
                let mut res = Vec::with_capacity(len);
                unsafe { res.set_len(len) };
                unsafe { std::ptr::write_unaligned(res.as_mut_ptr() as *mut u64, n) };
                res
            }
        }
    }
}

#[derive(Debug)]
pub enum Request {
    NewLine,
    Number(Number, Endianness),
}

impl Request {
    pub fn to_res(&self) -> Vec<u8> {
        match self {
            &Self::Number(n, e) => n.convert(e).into_vec(),
            Self::NewLine => {
                let mut res = Vec::with_capacity(1);
                res.push('\n' as u8);
                res
            }
        }
    }

    pub fn parse(mut raw: &str) -> Self {
        if raw.starts_with("Please send me the number ") {
            raw = &raw[26..];
            let n: &str;
            {
                let mut it = raw.splitn(2, ' ');
                n = it.next().unwrap();
                raw = it.next().unwrap();
            }
            assert!(raw.starts_with("as a "));
            raw = &raw[5..];
            let bit_s;
            let endian = {
                let mut it = raw.splitn(3, ' ');
                bit_s = it.next().unwrap();
                match it.next().unwrap() {
                    "big-endian" => Endianness::Big,
                    "little-endian" => Endianness::Little,
                    _ => unreachable!(),
                }
            };

            let n = match bit_s {
                "32-bit" => Number::U32(n.parse::<u32>().unwrap()),
                "64-bit" => Number::U64(n.parse::<u64>().unwrap()),
                _ => unreachable!(),
            };

            Self::Number(n, endian)
        } else if raw.starts_with("Please send me an empty line") {
            Self::NewLine
        } else {
            unreachable!()
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut conn = TcpStream::connect(ENDPOINT)?;
    let mut stream = std::io::BufReader::new(conn.try_clone().unwrap()).lines();
    while {
        if let Some(line) = stream.next() {
            let line = line?;
            println!("{}", line);
            let req = Request::parse(line.as_str());
            println!("{:?}", req);
            conn.write(req.to_res().as_slice()).unwrap();
            println!("{}", stream.next().unwrap().unwrap());
            true
        } else {
            false
        }
    } {}
    Ok(())
}
