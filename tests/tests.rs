use proc_unroll::unroll;

macro_rules! make_test {
    ($name:ident -> $type:ty, { $($body:tt)* }) => {
        #[test]
        fn $name() {
            fn normal() -> $type {
                $($body)*
            }

            #[unroll]
            fn unrolled() -> $type {
                $($body)*
            }

            assert_eq!(normal(), unrolled());
        }
    };
}

make_test!(simple -> Vec<isize>, {
    let mut vec = vec![];
    for x in 0..5 {
        vec.push(x);
    }
    vec
});

make_test!(negative -> Vec<isize>, {
    let mut vec = vec![];
    for x in -2..5 {
        vec.push(x);
    }
    vec
});

make_test!(suffixed -> Vec<isize>, {
    let mut vec = vec![];
    for x in 0..5usize {
        vec.push(x as isize);
    }
    vec
});

make_test!(slice -> Vec<isize>, {
    let mut vec = vec![];
    for &x in &[2, 4, 6, 8] {
        vec.push(x);
    }
    vec
});

#[test]
fn const_fn_range() {
    #[unroll]
    const fn inner() -> usize {
        let mut total = 0;
        for x in 1..5 {
            total += x;
        }
        total
    }

    assert_eq!(inner(), 10);
}

#[test]
fn const_fn_slice() {
    #[unroll]
    const fn inner() -> i32 {
        let mut total = 0;
        for x in &[5, 15] {
            total += *x;
        }
        total
    }

    assert_eq!(inner(), 20);
}
