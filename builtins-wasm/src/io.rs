// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use wasi::wasi_snapshot_preview1 as wasip1;

pub const STDOUT: i32 = 1;
pub const STDERR: i32 = 2;

/// Writes once, returning the WASI errno and the number of bytes written.
pub fn write(fd: i32, bytes: &[u8]) -> (i32, usize) {
    let mut written = 0usize;
    let iovec = wasi::Ciovec {
        buf: bytes.as_ptr(),
        buf_len: bytes.len(),
    };
    let errno = unsafe {
        wasip1::fd_write(
            fd,
            &iovec as *const _ as i32,
            1,
            &mut written as *mut _ as i32,
        )
    };
    (errno, written)
}

/// Writes all `bytes`, stopping early on an error, returning the WASI errno.
/// A write of no bytes stops with `EIO`.
pub fn write_all(fd: i32, mut bytes: &[u8]) -> i32 {
    while !bytes.is_empty() {
        match write(fd, bytes) {
            (0, n) if n > 0 => bytes = &bytes[n.min(bytes.len())..],
            (0, _) => return wasi::ERRNO_IO.raw() as i32,
            (errno, _) => return errno,
        }
    }
    0
}

pub fn random_seed() -> [u64; 4] {
    let mut seed = [0u64; 4];
    unsafe {
        let _ = wasi::random_get(seed.as_mut_ptr().cast(), 32);
    }
    if seed == [0; 4] {
        seed[0] = 0x9E37_79B9_7F4A_7C15;
    }
    seed
}
