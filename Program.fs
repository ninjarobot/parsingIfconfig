open FParsec

let pDot = pstring "."

let pIpv4 = many ((puint8 .>>? pDot) <|> puint8) 

let pIpElemn = (puint8 .>>? pDot) <|> puint8

let pIpv4_2 = pIpElemn .>>. pIpElemn .>>. pIpElemn .>>. pIpElemn

type IPv4 = IPv4 of uint8 * uint8 * uint8 * uint8
with
    static member FromParser (((octet1, octet2), octet3), octet4) =
        IPv4 (octet1, octet2, octet3, octet4)

let parsedIpV4 =
    pIpv4_2 |>> IPv4.FromParser
    //match pIpv4_2 s with
    //| FParsec.Reply (((octet1, octet2), octet3), octet4) ->
    //    IPV4(octet1, octet2, octet3, octet5)


let pIpv4_3 = sepBy puint8 pDot

// (puint8 .>> pDot >>. puint8 .>> pDot >>. puint8 .>> pDot >>. puint8)

// sepBy puint8 pDot

[<EntryPoint>]
let main argv =
    let ip = run pIpv4 "1.2.3.255.4."
    printfn "%A" ip
    let ip2 = run pIpv4_2 "1.2.3.255.4"
    printfn "%A" ip2
    let ip3 = run parsedIpV4 "1.2.3.255.4"
    printfn "%A" ip3
    let ip4 = run pIpv4_3 "1.2.3.255.4"
    printfn "%A" ip4
    0
