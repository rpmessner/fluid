defmodule Liquid.BlankFileSystem do
  def read_template_file(_, _, _) do
    { :error, "This liquid context does not allow includes." }
  end
end

defmodule Liquid.LocalFileSystem do
  def read_template_file(_, _, _) do
    { :ok, "" }
  end

  def full_path(root, template_path) do
    full_path = if Regex.match?(~r/\//, template_path) do
      root |> Path.join(template_path |> Path.dirname)
           |> Path.join("_#{template_path |> Path.basename}.liquid")
    else
      root |> Path.join("_#{template_path}.liquid")
    end
    cond do
      !Regex.match?(~r/^[^.\/][a-zA-Z0-9_\/]+$/, template_path) ->
        { :error, "Illegal template name '#{template_path}'" }
      !Regex.match?(~r/^#{Path.expand(root)}/, Path.expand(full_path)) ->
        { :error, "Illegal template path '#{Path.expand(full_path)}'" }
      true -> { :ok, full_path }
    end
  end
end

defmodule Liquid.FileSystem do
  use GenServer

  def full_path(path) do
    case lookup do
      nil -> { :error, "No file system defined" }
      { mod, root } -> mod.full_path(root, path)
    end
  end

  def read_template_file(path, options \\ []) do
    case lookup do
      nil -> { :error, "No file system defined" }
      { mod, root } -> mod.read_template_file(root, path, options)
    end
  end

  def handle_call({ :lookup }, _from, current) do
    { :reply, current, current }
  end

  def handle_call(:stop, _from, module) do
    { :stop, :normal, :ok, module }
  end

  def handle_cast({ :register, module, path }, _) do
    { :noreply, { module, path } }
  end

  def register(module), do: register(module, "")
  def register(module, path) do
    :gen_server.cast(__MODULE__, { :register, module, path })
  end

  def lookup do
    :gen_server.call(__MODULE__, { :lookup })
  end

  def start do
    :gen_server.start({ :local, __MODULE__ }, __MODULE__, { nil, nil }, [])
  end

  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

end
