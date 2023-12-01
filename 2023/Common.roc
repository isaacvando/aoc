interface Common
    exposes [dbge, unwrap]
    imports []


dbge = \x, label->
    dbg T label x
    x

unwrap = \r -> 
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val
