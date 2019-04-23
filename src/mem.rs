pub type ReadCallback = fn(addr: u16) -> Option<(u16, u8)>;
pub type WriteCallback = fn(addr: u16, value: u8) -> Option<(u16, u8)>;

pub struct Memory {
    bytes: [u8; 64 * 1024],
    read_callback: Option<ReadCallback>,
    write_callback: Option<WriteCallback>,
}

impl Default for Memory {
    fn default() -> Self {
        Self::new()
    }
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            bytes: [0; 64 * 1024],
            read_callback: None,
            write_callback: None,
        }
    }

    pub fn load(&mut self, addr: u16, data: &[u8]) {
        let program_end_addr = data.len() + addr as usize;
        self.bytes[(addr as usize)..program_end_addr].clone_from_slice(data);
    }

    pub fn register_read_callback(&mut self, cb: ReadCallback) {
        self.read_callback = Some(cb);
    }

    pub fn register_write_callback(&mut self, cb: WriteCallback) {
        self.write_callback = Some(cb);
    }

    pub fn set(&mut self, addr: u16, value: u8) {
        self.bytes[addr as usize] = value;

        if let Some(cb) = self.write_callback {
            self.process_callback(cb(addr, value));
        }
    }

    fn process_callback(&mut self, result: Option<(u16, u8)>) {
        if let Some((addr, value)) = result {
            self.bytes[addr as usize] = value;
        }
    }

    pub fn get(&mut self, addr: u16) -> u8 {
        let value = self.bytes[addr as usize];

        if let Some(cb) = self.read_callback {
            self.process_callback(cb(addr));
        }

        value
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
}
