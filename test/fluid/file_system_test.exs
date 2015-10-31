Code.require_file "../../test_helper.exs", __ENV__.file

defmodule FileSystemTest do
  use ExUnit.Case

  alias Fluid.FileSystem, as: FileSystem

  setup_all do
    Fluid.start
    on_exit fn -> Fluid.stop end
    :ok
  end

  test :default do
    FileSystem.register Fluid.BlankFileSystem, "/"
    { :error, _reason } = FileSystem.read_template_file("dummy", [dummy: "smarty"])
  end

  test :local do
    FileSystem.register Fluid.LocalFileSystem, "/some/path"

    { :ok, path } = FileSystem.full_path("mypartial")
    assert "/some/path/_mypartial.fluid" == path

    { :ok, path } = FileSystem.full_path("dir/mypartial")
    assert "/some/path/dir/_mypartial.fluid" == path

    { :error, _reason } = FileSystem.full_path("../dir/mypartial")

    { :error, _reason } =  FileSystem.full_path("/dir/../../dir/mypartial")

    { :error, _reason } =  FileSystem.full_path("/etc/passwd")

  end
end
