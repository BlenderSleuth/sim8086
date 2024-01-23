pub fn combine_u8s_to_u16(hi: u8, lo: u8) -> u16 {
    (hi as u16) << 8 | (lo as u16)
}

// Splits 16-bit value into (hi, lo)
pub fn split_u16_to_u8s(value: u16) -> (u8, u8) {
    ((value >> 8) as u8, value as u8)
}