import sys


def test_fib():
    result = open("./fib_result", "r").read().strip()
    assert 9227465 == int(
        eval(result)
    ), f"Failed fib\n expected 9227465 got {int(result)}"
    print("Successfully tested fib")


def test_logical():
    result = open("./logical_result").read().split("\n")

    assert (
        "false" == result[0]
    ), f"Failed 'print false;'; Expected 'false' but got {result[0]}"

    assert (
        "false" == result[1]
    ), f"Failed 'print 5 < 3;'; Expected 'false' but got {result[1]}"

    assert (
        "false" == result[2]
    ), f"Failed 'print 5 <= 3;'; Expected 'false' but got {result[2]}"

    assert (
        "false" == result[3]
    ), f"Failed 'print 5 == 3;'; Expected 'false' but got {result[3]}"

    assert (
        "true" == result[4]
    ), f"Failed 'print 5 > 3;'; Expected 'false' but got {result[4]}"

    assert (
        "true" == result[5]
    ), f"Failed 'print 5 >= 3;'; Expected 'false' but got {result[5]}"

    assert (
        "true" == result[6]
    ), f"Failed 'print !nil;'; Expected 'false' but got {result[6]}"

    assert (
        "false" == result[7]
    ), f"Failed 'nil != nil;'; Expected 'false' but got {result[7]}"

    assert (
        "true" == result[8]
    ), f"Failed '5 * 2 < 3 * 5'; Expected 'false' but got {result[8]}"

    assert (
        "false" == result[9]
    ), f"Failed '5 + 5 * 2 < 3 * 5'; Expected 'false' but got {result[9]}"

    print("Successfully tested logical")


def test_vm():
    result = open("./vm_result").read().split("\n")

    assert 5 == int(
        eval(result[0])
    ), f"Failed 'print 5;'; Expected '5' but got {int(eval(result[0]))}"

    assert [
        0,
        1,
        2,
        3,
        4,
    ] == [
        int(eval(i)) for i in result[1:6]
    ], f"Failed 'for(var i = 0; i < 5; i = i + 1){{print i;}}'; Expected '[0,1,2,3,4]' but got {[int(eval(i)) for i in result[1:5]]}"

    assert [
        0,
        1,
        2,
        3,
        4,
    ] == [
        int(eval(i)) for i in result[6:11]
    ], f"Failed 'var i = 0; while(i < 5){{print i; i = i + 1}}'; Expected '[0,1,2,3,4]' but got {[int(eval(i)) for i in result[6:11]]}"

    assert 5 == int(
        eval(result[11])
    ), f"Failed 'fun fib(a){{if(a <= 2){{return 1;}} return fib(a-1) + fib(a-2);}} print fib(5);'; Expected '5' but got {eval(result[11])}"

    assert (
        "nil" == result[12]
    ), f"Failed 'var i = 0; while(i < 5){{print i; i = i + 1}}'; Expected '[0,1,2,3,4]' but got {[int(eval(i)) for i in result[6:11]]}"

    assert ["point struct", "point instance", 5, 10] == result[13:15] + [
        eval(result[15]),
        eval(result[16]),
    ], f"Failed 'struct point{{x;y;}};print(point); var p = point(5,10); print(p); print(p.x); print(p.y);'; Expected '[point struct, point instance, 5, 10]' but got {result[13:15] + [ int(eval(result[15])), int(eval(result[16]))]}"

    assert [1, 2, 3] == [
        int(i) for i in eval(result[17])
    ], f"Failed 'var m = [1,2,3]; print m;'; Expected '[1,2,3]' but got {[int(eval(i)) for i in result[17:19]]}"

    assert 5 == int(
        eval(result[18])
    ), f"Failed 'var m = [1,2,3]; m[1] = 5; print m[1];'; Expected 5 but got {int(eval(result[18]))}"

    assert {"5": 12.0, "b": 2.0, "a": 1.0} == eval(
        result[19]
    ), f"Failed 'var m = {{'a':1, 'b': 2, '5':12}}; print m;'; Expected {{'5': 12.0, 'b': 2.0, 'a': 1.0}} but got {eval(result[19])}"

    assert 1 == eval(
        result[20]
    ), f"Failed 'var m = {{'a':1, 'b': 2, '5':12}}; print m['a'];'; Expected 1 but got {eval(result[20])}"

    assert 5 == eval(
        result[21]
    ), f"Failed 'var m = {{'a':1, 'b': 2, '5':12}}; m['c'] = 5; print m['c'];'; Expected 5 but got {eval(result[21])}"

    assert 12 == eval(
        result[22]
    ), f"Failed 'var m = {{'a':1, 'b': 2, '5':12}}; print m['5'];'; Expected 12 but got {eval(result[22])}"

    print("Successfully tested vm")


if __name__ == "__main__":
    if sys.argv[1] == "fib":
        test_fib()
    elif sys.argv[1] == "logical":
        test_logical()
    elif sys.argv[1] == "vm":
        test_vm()
