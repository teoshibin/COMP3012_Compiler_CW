let var n := 10;
    var a := 0;
    var b := 1;
    var i;
    var fib
in begin
  i := 0;
  while i <= n do
    begin
      fib := a;
      a := b;
      b := fib + a;
      i := i+1
    end;
  printint (fib)
end