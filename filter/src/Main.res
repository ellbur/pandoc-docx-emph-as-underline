
type stream
type nodeProcess = {
  stdin: stream,
  stdout: stream
}

@val external process: nodeProcess = "process"
@send external resume: stream => () = "resume"
@send external setEncoding: (stream, string) => () = "setEncoding"
@send external inChannelOnString: (stream, string, string => ()) => () = "on"
@send external inChannelOnUnit: (stream, string, () => ()) => () = "on"
@send external write: (stream, string) => () = "write"

process.stdin->resume
process.stdin->setEncoding("utf8")

let inputChunks = [ ]

process.stdin->inChannelOnString("data", chunk => {
  inputChunks->Js.Array2.push(chunk)->ignore
})

@val @scope("Object") external jsUpdate: (@as(json`{}`) _, Js.Dict.t<'a>, Js.Dict.t<'a>) => Js.Dict.t<'a> = "assign"

let update = (dict, key, newValue) => {
  jsUpdate(dict, Js_dict.fromArray([(key, newValue)]))
}

let mapAt = (dict, key, f) => {
  switch dict->Js_dict.get(key) {
    | None => dict
    | Some(x) => {
      dict->update(key, f(x))
    }
  }
}

let attrTest = (dict, key, f) => dict->Js_dict.get(key)->Belt.Option.mapWithDefault(false, f)
let tIs = (props, t) => props->attrTest("t", t2 => t2->Js.Json.decodeString->Belt.Option.mapWithDefault(false, s => s == t))
  
let propsCExn = props =>
  props->Js_dict.get("c")->Belt.Option.getExn
  
let rec filterC: Js.Json.t => Js.Json.t = n => {
  open Js.Json
  switch classify(n) {
    | JSONObject(props) if props->tIs("Emph") => {
      object_(Js_dict.fromArray([
        ("t", string("Span")),
        ("c", array([
          array([
            string(""),
            array([string("underline")]),
            array([])
          ]),
          filterC(propsCExn(props))
        ]))
      ]))
    }
    | JSONObject(props) => {
      props->mapAt("c", filterC)->object_
    }
    | JSONArray(elems) => {
      elems->Js.Array2.map(filterC)->array
    }
    | _ => n
  }
}

let filter = root => {
  open Js.Json
  open Belt.Option
  root->decodeObject->getExn->mapAt("blocks", blocks => {
    blocks->decodeArray->getExn->Js.Array2.map(filterC)->array
  })->object_
}

process.stdin->inChannelOnUnit("end", () => {
  open! Js.Console
  let joinWith = Js.Array2.joinWith
  let everything = inputChunks->joinWith("")
  let parsed = Js.Json.parseExn(everything)
  let filtered = filter(parsed)
  let out = Js.Json.stringify(filtered)
  process.stdout->write(out)
})

