let var n := 12;
    var x := 0;
    var y := 1;
    var z := x;
    var i := 1
in
begin
  while i <= n do
    begin
      z := x + y;
      y := x;
      x := z;
      printint (x);
      getint (x);
      i := i + 1
    end
end