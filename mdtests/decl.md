# static_int

```c
static int x;
```

```rust
#[no_mangle]
pub static mut x: c_int = 0;
```

# static_pointer

```c
typedef int *ip;
static ip x;
```

```rust
#[no_mangle]
pub static mut x: *mut c_int = (0) as *mut c_int;
```

# extern_var

```c
extern char x;
```

```rust
extern {
    #[no_mangle]
    pub static mut x: c_char;
}
```

# struct_simple

```c
extern struct a { int b; } c;
```

```rust
extern {
    pub static mut c: a;
}
#[repr(C)]
pub struct a {
    pub b: c_int,
}
```

# struct_anon

```c
struct { union { int a; float b; }; struct { int c, d; }; } v;
```

```rust
#[no_mangle]
pub static mut v: Generated_0 = Generated_0 {
    anon_0: Generated_1 {
        a: (0) as c_int,
    },
    anon_1: Generated_2 {
        c: (0) as c_int,
        d: (0) as c_int,
    },
};
#[repr(C)]
pub struct Generated_0 {
    pub anon_0: Generated_1,
    pub anon_1: Generated_2,
}
#[repr(C)]
pub union Generated_1 {
    pub a: c_int,
    pub b: c_float,
}
#[repr(C)]
pub struct Generated_2 {
    pub c: c_int,
    pub d: c_int,
}
```

# struct_anon_clobber

```c
int Generated_0;
struct {
    int anon_0;
    struct {
        int anon_1;
    };
} Generated_1;
struct Generated_0 *p;
```

```rust
#[no_mangle]
pub static mut Generated_0: c_int = (0) as c_int;
#[no_mangle]
pub static mut Generated_1: Generated_2 = Generated_2 {
    anon_0: (0) as c_int,
    anon_1: Generated_3 {
        anon_1: (0) as c_int,
    },
};
#[no_mangle]
pub static mut p: *mut Generated_0_ = (0) as *mut Generated_0_;
#[repr(C)]
pub struct Generated_2 {
    pub anon_0: c_int,
    pub anon_1: Generated_3,
}
#[repr(C)]
pub struct Generated_3 {
    pub anon_1: c_int,
}
pub enum Generated_0_{}
```

# literal_int

```c
extern int a = 1;
```

```rust
#[no_mangle]
pub static mut a: c_int = (1) as c_int;
```

# literal_int_suffix

```c
unsigned long long a = 1llu;
```

```rust
#[no_mangle]
pub static mut a: c_ulonglong = (1) as c_ulonglong;
```

# literal_float

```c
double f = 0.1;
```

```rust
#[no_mangle]
pub static mut f: c_double = (0.1) as c_double;
```

# literal_float_suffix

```c
float f = 0.1f;
```

```rust
#[no_mangle]
pub static mut f: c_float = (0.1) as c_float;
```

# literal_float_suffix_coerce

```c
double f = 0.1f;
```

```rust
#[no_mangle]
pub static mut f: c_double = ((0.1) as c_float) as c_double;
```

# expr_simple

```c
int f = -1 + 1;
```

```rust
#[no_mangle]
pub static mut f: c_int = (-((1) as c_int)) + ((1) as c_int);
```


# expr_integer_zero

```c
int x = 0;
```

```rust
#[no_mangle]
pub static mut x: c_int = (0) as c_int;
```

# expr_pointer_zero

```c
int *p = 0;
```

```rust
#[no_mangle]
pub static mut p: *mut c_int = ((0) as c_int) as *mut c_int;
```

# integer_literal_expand

This test assumes that `1000` is too large to fit a `char`.

```c
char x = 1000;
```

```rust
#[no_mangle]
pub static mut x: c_char = ((1000) as c_int) as c_char;
```
