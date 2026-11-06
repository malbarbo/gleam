// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

//! Bump arena living in linear memory, after `__heap_base`.
//!
//! Memory is only reclaimed by `release`, which the generated glue calls with
//! the mark it took before lowering the arguments of a call.

use core::alloc::{GlobalAlloc, Layout};
use core::arch::wasm32;

const PAGE: usize = 65536;

unsafe extern "C" {
    static __heap_base: u8;
}

// Zero means "not initialized yet"; the first use sets it to the heap base.
static mut TOP: usize = 0;

fn align_up(n: usize, align: usize) -> Option<usize> {
    Some(n.checked_add(align - 1)? & !(align - 1))
}

/// The start of the arena, right after the data of this crate.
pub fn base() -> usize {
    (&raw const __heap_base as usize).next_multiple_of(8)
}

pub fn top() -> usize {
    unsafe {
        if TOP == 0 {
            TOP = base();
        }
        TOP
    }
}

pub fn release(mark: usize) {
    unsafe { TOP = mark }
}

/// Makes sure `end` is addressable, growing the memory if needed. The size
/// in bytes is computed in `u64`, as the 4 GiB of a full memory do not fit in
/// `usize`.
fn ensure(end: usize) -> bool {
    let size = wasm32::memory_size(0) as u64 * PAGE as u64;
    if end as u64 <= size {
        return true;
    }
    let pages = (end as u64 - size).div_ceil(PAGE as u64);
    wasm32::memory_grow(0, pages as usize) != usize::MAX
}

/// Allocates `size` bytes aligned to `align` (a power of two), or returns null.
pub fn alloc(size: usize, align: usize) -> *mut u8 {
    let Some(start) = align_up(top(), align.max(1)) else {
        return core::ptr::null_mut();
    };
    match start.checked_add(size) {
        Some(end) if ensure(end) => {
            unsafe { TOP = end };
            start as *mut u8
        }
        _ => core::ptr::null_mut(),
    }
}

/// Resizes an allocation, in place when it is the last one.
///
/// # Safety
/// `ptr` must be null or a live allocation of `old_size` bytes.
pub unsafe fn realloc(ptr: *mut u8, old_size: usize, align: usize, new_size: usize) -> *mut u8 {
    if ptr.is_null() {
        return alloc(new_size, align);
    }
    let start = ptr as usize;
    if start.checked_add(old_size) == Some(top()) {
        let Some(end) = start.checked_add(new_size) else {
            return core::ptr::null_mut();
        };
        if !ensure(end) {
            return core::ptr::null_mut();
        }
        unsafe { TOP = end };
        return ptr;
    }
    if new_size <= old_size {
        return ptr;
    }
    let new = alloc(new_size, align);
    if !new.is_null() {
        unsafe { core::ptr::copy_nonoverlapping(ptr, new, old_size) };
    }
    new
}

pub struct Arena;

unsafe impl GlobalAlloc for Arena {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        alloc(layout.size(), layout.align())
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {}

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        unsafe { realloc(ptr, layout.size(), layout.align(), new_size) }
    }
}
