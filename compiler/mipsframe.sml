signature FRAME =
sig 
    type frame
    type access
    val newFrame : {name: Temp.label,
                    formals: bool list} -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
end

structure MipsFrame : FRAME = struct
    (*TODO add view shift*)
    type frame = {formals: access list, shift: unit, locals: int ref, 
                  label: Temp.label}
    datatype access = InFrame of int | InReg of Temp.temp
    fun newFrame{name=name,formals=formals} = 
        let fun locateFormals 0 = [InFrame 0]
            |   locateFormals n = InFrame(n)::locateFormals(n-1)
        in {formals=locateFormals(length formals) , shift=(), locals= ref 0, 
            label=name}
        end
    fun name frame = #name frame
    fun formals frame = #formals frame
    fun allocLocal frame bool = 
       let val locals = #locals frame
       in (locals := locals + 1; ~locals)
       end 
end

structure Frame : FRAME = MipsFrame