locals_without_parens = [rule: 2, rule?: 2]

[
  inputs: ["{mix,.formatter,.credo}.exs", "{lib,test}/**/*.{ex,exs}"],
  line_length: 135,
  locals_without_parens: locals_without_parens,
  export: [
    locals_without_parens: locals_without_parens
  ]
]
