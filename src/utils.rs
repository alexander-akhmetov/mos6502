pub fn little_endian_to_u16(bigger: u8, little: u8) -> u16 {
    // Add parentheses for clarity, as suggested by clippy::precedence
    (u16::from(bigger) << 8) | u16::from(little)
}
