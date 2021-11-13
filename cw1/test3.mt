let var n
in begin
  getint (n);
  while n>1 do begin
    if 2 * (n/2) == n 
      then n := n/2
      else n := 3*n+1;
    printint (n)
  end
end