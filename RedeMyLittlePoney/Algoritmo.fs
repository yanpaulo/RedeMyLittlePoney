namespace RedeMyLittlePoney

open System
open FSharp.Data
open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.LinearAlgebra
open System.Diagnostics

module Algoritmo =
    //Tipos
    type Par = { X: float Vector; Y: float Vector }
    type Modelo = { I: float Vector list; J: float Vector list }
    type Realizacao = { TaxaAcerto:float; Confusao: float Matrix; W: Modelo }
    type ResultadoAlgoritmo = { Acuracia: float; Tempo: TimeSpan; Melhor: Realizacao; Dados: Par seq; }
    
    //Funções
    let sigmoide x = 
        1.0 /  (1.0 + Math.Pow(Math.E, -x))
    
    let sigmoide' x = 
        Differentiate.derivative 1 x sigmoide
    
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
    let pesos treinamento =
        (treinamento: Par list) |> ignore

        //Máximo de épocas
        let maxN = 500
        let taxa = 0.15
        
        //Próximo modelo (função w(n+1))
        let rec proximo t m = 
            (m: Modelo) |> ignore
            match t with
                | [] -> m
                | par :: tail -> 
                    let xj = saidaCamadaI m.I par.X
                    let y = saidaCamadaJ m.J xj
                    let erro = par.Y - y

                    let wjMap j wj = 
                        let e = erro.[j]
                        let f'u = sigmoide' (ponderada wj xj)
                        //let h = saida m.I.[j] par.X
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

                            let map j wj = e j * f'u j * wji j
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
        
        let numNeuronios = 5
        let numClasses = 3

        let rv n = 
            let f _ = Random.doubles n |> vector
            f
        
        let li = rv treinamento.Head.X.Count |> List.init numNeuronios 

        let lj = rv (numNeuronios + 1) |> List.init numClasses 
        
        let m0 = { I = li; J = lj }

        //Inicia o treinamento
        pesos m0 0

    let realizacao dados =
        let confusao = DenseMatrix.zero 3 3
    
        let treinamento = 
            let n = dados |> List.length |> float |> (*) 0.8 |> int
            dados |> List.take n

        let teste = dados |> List.except treinamento

        let w = pesos treinamento

        let classes = dict[[1.0; 0.0; 0.0] |> vector, 0; [0.0; 1.0; 0.0] |> vector, 1; [0.0; 0.0; 1.0] |> vector, 2]

        teste |>
            Seq.iter (fun par -> 
                let a = resultado w par.X
                if classes.ContainsKey a then (confusao.[classes.[a], classes.[par.Y]] <- confusao.[classes.[a], classes.[par.Y]] + 1.0)
                )
        
        { TaxaAcerto = confusao.Diagonal().Sum() / float (teste |> Seq.length) ; Confusao = confusao; W = w }

    let algoritmoIris =
        let db = CsvFile.Load("iris.data").Cache()
        let classes = dict["Iris-setosa", [1.0; 0.0; 0.0]; "Iris-versicolor", [0.0; 1.0; 0.0]; "Iris-virginica", [0.0; 0.0; 1.0]]

        let parse (s: string) = s.Replace(".", ",") |> System.Double.Parse

        let normaliza x =
            let parseRow (row: CsvRow) = row.Columns|> Seq.take 4 |> Seq.map parse
            let valores = db.Rows |> Seq.collect parseRow
            let min = valores |> Seq.min
            let max = valores |> Seq.max

            (x - min) / (max - min)
    
        let normaliza s = parse s |> normaliza

        let parseRow (row: CsvRow) = row.Columns |> Seq.take 4 |> Seq.map normaliza |> List.ofSeq

        let mapRow (row: CsvRow) = { X = parseRow row |> vector; Y = classes.[row.["class"]] |> vector }
    
        let dados = db.Rows |> Seq.map mapRow |> List.ofSeq

        let realizacoes =
            [1..5] |>
            Seq.map (fun _ -> realizacao (dados.SelectPermutation() |> List.ofSeq)) |>
            List.ofSeq
    
        let maior = 
            realizacoes |>
            Seq.maxBy (fun r -> r.TaxaAcerto)
        
        let media =
            realizacoes |>
            Seq.averageBy (fun r -> r.TaxaAcerto)
        
        //printfn "%A %A" media maior

        { Acuracia = media; Tempo = TimeSpan.Zero; Melhor = maior; Dados = dados; }

