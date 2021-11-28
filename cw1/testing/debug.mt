let var n := 0;
    var x := 0;
    var y := 1;
    var z := x;
    var i := 1
in
begin
  getint (n);
  while i <= n do
    begin
      printint (x);
      z := x + y;
      y := x;
      x := z;
      i := i + 1
    end
end