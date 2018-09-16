namespace RedeMyLittlePoney

open System
open System.Diagnostics
open System.Collections.Generic

open FSharp.Data
open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.LinearAlgebra
open FSharp.Collections.ParallelSeq

module Algoritmo =

    //Tipos
    type Par = { X: float Vector; Y: float Vector }
    type Modelo = { I: float Vector list; J: float Vector list }
    type Realizacao = { TaxaAcerto:float; Confusao: float Matrix; W: Modelo }

    type ResultadoAlgoritmo = { Acuracia: float; Melhor: Realizacao; }
    type ResultadoParametros = { NumeroNeuronios: int; TaxaAprendizado: float; Precisao: float }
    
    //Funções
    let sigmoide x = 
        1.0 /  (1.0 + Math.Pow(Math.E, -x))
    
    let sigmoide' x = 
        //Differentiate.derivative 1 x sigmoide
        (sigmoide x) * (1.0 - (sigmoide x))
    
    let ponderada w x =
        x .* w |> Vector.sum

    let saida w x =
        ponderada w x |> sigmoide

    let saidaCamadaI c x =
        let map w = saida w x
        let saida = c |> List.map map
        -1.0 :: saida |> vector
        
    let saidaCamadaJ c x =
        let map w = saida w x
        c |> Seq.map map |> vector
    
    let resultado m x =
        let xj = saidaCamadaI m.I x
        let y = saidaCamadaJ m.J xj
        y |> Seq.map (fun n -> Math.Round n) |> vector

    //Próximo modelo para o vetor "treinamento"
    let pesos treinamento neuronios taxa classes =
        (treinamento: Par list) |> ignore

        //Épocas
        let maxN = 200
        
        //Próximo modelo (função w(n+1))
        let rec proximo t m = 
            match t with
                | [] -> m
                | par :: tail -> 
                    let xj = saidaCamadaI m.I par.X
                    let y = saidaCamadaJ m.J xj
                    let erro = par.Y - y

                    let wjMap j wj = 
                        let e = erro.[j]
                        let f'u = sigmoide' (ponderada wj xj)
                        let h = xj

                        let ajuste = e * taxa * f'u * h
                        wj + ajuste

                    let wiMap i wi =
                        let h'u = ponderada wi par.X |> sigmoide'
                        let somatorio =
                            let wj j = m.J.[j]
                            let e j = erro.[j]
                            let f'u j = ponderada (wj j) xj |> sigmoide'
                            let wji j = (wj j).[i]

                            let map j _ = e j * f'u j * wji j
                            m.J |> List.mapi map |> List.sum

                        let ajuste = taxa * h'u * somatorio * par.X
                        wi + ajuste

                    let j1 = m.J |> List.mapi wjMap
                    let i1 = m.I |> List.mapi wiMap

                    let m1 = { I = i1; J = j1 }

                    proximo tail m1
    
        //Decide se os pesos ainda devem ser atualizados (por número de épocas)
        let rec pesos m n =
            let w1 = proximo (treinamento.SelectPermutation() |> List.ofSeq) m

            if n < maxN then pesos w1 (n+1) else w1
        
        let vetorAleatorioFn n = 
            let f _ = Random.doubles n |> vector
            f
        
        let li = vetorAleatorioFn treinamento.Head.X.Count |> List.init neuronios 

        let lj = vetorAleatorioFn (neuronios + 1) |> List.init classes 
        
        let m0 = { I = li; J = lj }

        //Inicia o treinamento
        pesos m0 0

    let realizacao dados classes neuronios taxa =
        let numClasses = classes |> List.length        
        let confusao = DenseMatrix.zero numClasses numClasses
    
        let treinamento = 
            let n = dados |> List.length |> float |> (*) 0.8 |> int
            dados |> List.take n

        let teste = dados |> List.except treinamento

        let w = pesos treinamento neuronios taxa numClasses
        
        let iter par =
            let y = resultado w par.X
            let index = classes |> List.tryFindIndex (fun e -> e = y)

            match index with
                | Some i -> 
                    let j = classes |> List.findIndex (fun e -> e = par.Y)
                    confusao.[i, j] <- confusao.[i, j] + 1.0
                | None -> ()

        teste |> Seq.iter iter
        
        { TaxaAcerto = confusao.Diagonal().Sum() / float (teste |> Seq.length) ; Confusao = confusao; W = w }
    
    let precisao dados neuronios taxa classes = 
        (dados: Par list) |> ignore
        let secoes = 5
        let tamanhoSecao = dados.Length / secoes

        let precisaoSecao n =
            let head = dados |> List.take (tamanhoSecao * n)
            let secao = dados |> List.skip (tamanhoSecao * n) |> List.take tamanhoSecao
            let tail = dados |> List.skip (tamanhoSecao * (n + 1))

            let treinamento = head @ tail
            let teste = secao

            let m = pesos treinamento neuronios taxa classes
            let acertos = 
                teste |> 
                List.map (fun t -> resultado m t.X = t.Y) |>
                List.filter (fun r -> r) |>
                List.length |> float
            
            acertos / (float teste.Length)

        [0 .. (secoes - 1)] |> List.map precisaoSecao |> List.average

    let ajusteGrid dados classes neuronios taxas = 
        let combinacoes = List.allPairs neuronios taxas 
        
        let map (neuronios, taxa) =
            let precisao = precisao dados neuronios taxa classes
            let mapping = { NumeroNeuronios = neuronios; TaxaAprendizado = taxa; Precisao = precisao }
            printfn "%A" mapping
            mapping
            
        combinacoes |> PSeq.map map |> PSeq.maxBy (fun r -> r.Precisao)
    
    let sw = new Stopwatch();

    let normaliza x min max =
        (x - min) / (max - min)

    let algoritmo dados classes neuronios taxas = 
        let numClasses = classes |> List.length

        printfn "Busca de parâmetros em grade\n"
        sw.Start()
        let parametros = ajusteGrid dados numClasses neuronios taxas
        sw.Stop()
        printfn "\nParametros escolhidos: \n%A \n(%A)\n" parametros sw.Elapsed

        sw.Restart()
        printf "Fazendo realizacoes... "
        
        let map _ = 
            realizacao (dados.SelectPermutation() |> List.ofSeq) classes parametros.NumeroNeuronios parametros.TaxaAprendizado

        let realizacoes =
            [0 .. 20] |> PSeq.map map |> PSeq.toList
    
        let maior = 
            realizacoes |>
            List.maxBy (fun r -> r.TaxaAcerto)
        
        let media =
            realizacoes |>
            List.averageBy (fun r -> r.TaxaAcerto)
        
        sw.Stop()
        printfn "%A\n" sw.Elapsed

        { Acuracia = media; Melhor = maior; }
     
    let algoritmoCSV db classes colunas colunaClasse neuronios taxas =
        (db : Runtime.CsvFile<CsvRow>) |> ignore
        (classes: IDictionary<string, float Vector>) |> ignore
        (colunaClasse : int) |> ignore

        let parse (s: string) = s.Replace(".", ",") |> System.Double.Parse

        let normaliza x =
            let parseRow (row: CsvRow) = row.Columns|> Seq.take colunas |> Seq.map parse
            let valores = db.Rows |> Seq.collect parseRow
            let min = valores |> Seq.min
            let max = valores |> Seq.max

            normaliza x min max
    
        let normaliza s = parse s |> normaliza

        let parseRow (row: CsvRow) = row.Columns |> Seq.take colunas |> Seq.map normaliza |> List.ofSeq

        let mapRow (row: CsvRow) = { X = parseRow row |> vector; Y = classes.[row.[colunaClasse]] |> vector }
    
        let dados = db.Rows |> Seq.map mapRow |> List.ofSeq
        let classes = classes.Values |> Seq.map (fun e -> vector e) |> List.ofSeq

        algoritmo dados classes neuronios taxas

    let algoritmoIris () =
        printfn "Iris"
        let db = CsvFile.Load("iris.data").Cache()
        let classes = dict["Iris-setosa", vector [1.0; 0.0; 0.0]; "Iris-versicolor", vector [0.0; 1.0; 0.0]; "Iris-virginica", vector [0.0; 0.0; 1.0]]
        let taxas = [0.1 .. 0.1 .. 0.5]
        let neuronios = [4 .. 10]

        algoritmoCSV db classes 4 4 neuronios taxas

    let algoritmoColuna () =
        printfn "Coluna Terbreval"
        let db = CsvFile.Load("column_3C.dat", " ").Cache()
        let classes = dict["DH", vector [1.0; 0.0; 0.0]; "SL", vector [0.0; 1.0; 0.0]; "NO", vector [0.0; 0.0; 1.0]]
        let taxas = [0.2 .. 0.1 .. 0.5]
        let neuronios = [7 .. 10]

        algoritmoCSV db classes 6 6 neuronios taxas

