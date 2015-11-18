Code.require_file "../../test_helper.exs", __ENV__.file

defmodule FileSystemTest do
  use ExUnit.Case

  alias Liquescent.FileSystem, as: FileSystem

  setup_all do
    Liquescent.start
    on_exit fn -> Liquescent.stop end
    :ok
  end

  test :default do
    FileSystem.register Liquescent.BlankFileSystem, "/"
    { :error, _reason } = FileSystem.read_template_file("dummy", [dummy: "smarty"])
  end

  test :local do
    FileSystem.register Liquescent.LocalFileSystem, "/some/path"

    { :ok, path } = FileSystem.full_path("mypartial")
    assert "/some/path/_mypartial.liquescent" == path

    { :ok, path } = FileSystem.full_path("dir/mypartial")
    assert "/some/path/dir/_mypartial.liquescent" == path

    { :error, _reason } = FileSystem.full_path("../dir/mypartial")

    { :error, _reason } =  FileSystem.full_path("/dir/../../dir/mypartial")

    { :error, _reason } =  FileSystem.full_path("/etc/passwd")

  end
end
