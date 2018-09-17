namespace RedeMyLittlePoney

// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module Application =
    [<EntryPoint>]
    let main argv = 
        printfn "%A" (Algoritmo.algoritmoIris())
        printfn "%A" (Algoritmo.algoritmoColuna())
        printfn "%A" (Algoritmo.algoritmoDermatologia())
        printfn "%A" (Algoritmo.algoritmoCancer())
        0 // retornar um código de saída inteiro
