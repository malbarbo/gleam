// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

//! Reading arguments from and writing results to linear memory, following
//! the layout in `ABI.md`. Every value is 8-aligned and its size is a
//! multiple of 8; pointers and lengths are `u32`.

use alloc::string::String;
use core::alloc::Layout;
use core::slice;

/// # Safety
/// `ptr` and `len` must describe valid UTF-8 bytes in linear memory.
pub unsafe fn str_arg<'a>(ptr: i32, len: i32) -> &'a str {
    unsafe { core::str::from_utf8_unchecked(bytes_arg(ptr, len)) }
}

/// # Safety
/// `ptr` and `len` must describe bytes in linear memory.
pub unsafe fn bytes_arg<'a>(ptr: i32, len: i32) -> &'a [u8] {
    if len == 0 {
        return &[];
    }
    unsafe { slice::from_raw_parts(ptr as usize as *const u8, len as u32 as usize) }
}

/// The elements of a list, each `size` bytes.
///
/// # Safety
/// `ptr` and `count` must describe a list in linear memory.
pub unsafe fn list_arg(ptr: i32, count: i32, size: usize) -> impl Iterator<Item = *const u8> {
    (0..count as u32 as usize).map(move |i| (ptr as usize + i * size) as *const u8)
}

/// Writes into a return area or a list element.
#[derive(Clone, Copy)]
pub struct Out(pub *mut u8);

impl Out {
    pub fn new(ret: i32) -> Out {
        Out(ret as usize as *mut u8)
    }

    pub fn at(self, offset: usize) -> Out {
        Out(unsafe { self.0.add(offset) })
    }

    pub fn u32(self, v: u32) {
        unsafe { (self.0 as *mut u32).write(v) }
    }

    pub fn i64(self, v: i64) {
        unsafe { (self.0 as *mut i64).write(v) }
    }

    pub fn f64(self, v: f64) {
        unsafe { (self.0 as *mut f64).write(v) }
    }

    pub fn ptr_len(self, ptr: *const u8, len: usize) {
        self.u32(ptr as usize as u32);
        self.at(4).u32(len as u32);
    }

    /// A `String` pointing to `s`, which must outlive the glue call.
    pub fn str(self, s: &str) {
        self.ptr_len(s.as_ptr(), s.len());
    }

    /// A `String` owning `s`; its bytes stay in the arena until released.
    pub fn string(self, s: String) {
        self.str(s.leak());
    }

    /// A `List` of `items`, each written by `write` into `size` bytes.
    pub fn list<T>(
        self,
        items: impl ExactSizeIterator<Item = T>,
        size: usize,
        write: impl Fn(Out, T),
    ) {
        let count = items.len();
        let base = alloc_area(count * size);
        for (i, item) in items.enumerate() {
            write(base.at(i * size), item);
        }
        self.ptr_len(base.0, count);
    }

    /// `Ok(payload)`, the payload written by `write` at +8.
    pub fn ok(self, write: impl FnOnce(Out)) {
        self.u32(0);
        write(self.at(8));
    }

    pub fn error(self) {
        self.u32(1);
    }

    pub fn result<T>(self, value: Option<T>, write: impl FnOnce(Out, T)) {
        match value {
            Some(v) => self.ok(|out| write(out, v)),
            None => self.error(),
        }
    }
}

/// An 8-aligned area in the arena.
fn alloc_area(size: usize) -> Out {
    if size == 0 {
        return Out(8 as *mut u8);
    }
    let Ok(layout) = Layout::from_size_align(size, 8) else {
        panic!("allocation too large");
    };
    let ptr = unsafe { alloc::alloc::alloc(layout) };
    if ptr.is_null() {
        alloc::alloc::handle_alloc_error(layout);
    }
    Out(ptr)
}
