pub fn little_endian_to_u16(big: u8, little: u8) -> u16 {
    (little as u16) << 8 | (big as u16)
}
