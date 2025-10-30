python
import sys
import os

# Find rustc's sysroot
rustc_sysroot = os.popen("rustc --print sysroot").read().strip()
gdb_rust_dir = os.path.join(rustc_sysroot, "lib/rustlib/etc")

if os.path.exists(gdb_rust_dir):
    sys.path.insert(0, gdb_rust_dir)
    import gdb_rust_pretty_printing
    gdb_rust_pretty_printing.register_printers(gdb.current_objfile())
end

set print pretty on
set print object on
set print static-members on
set print vtbl on
set print demangle on
set demangle-style gnu-v3
