list = Enum.to_list(1..10_000)
map_fun = fn(i) -> [i, i * i] end

Liquid.start()

markup = File.read!("./bench/simple.liquid")

Benchee.run(%{"parse:" => fn ->
               markup
               |> Liquid.Template.parse()
             end,
              "parse and render:" => fn ->
                markup
                |> Liquid.Template.parse()
                |> Liquid.Template.render()
              end},
  warmup: 5,
  time: 60
)
