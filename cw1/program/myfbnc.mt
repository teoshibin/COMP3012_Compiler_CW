let var n := 5;
    var i;
    var x := 0;
    var y := 1;
    var z := x
in 
    begin 
        getint (n);
        i := 0;
        while i <= n do
            begin
            printint (x);
            z := x + y;
            x := y;
            y := z;
            i := i+1
            end
    end

