defmodule T3.Utils do
	
  defmodule SecureRandom do
		
    @default_length 16
		
    @doc """
    Returns random hex string.
    
    ## Examples
  
    iex> T3.Utils.SecureRandom.hex
    "c3d3b6cdab81a7382fbbae33407b3272"

    iex> T3.Utils.SecureRandom.hex(8)
    "125583e32b698259"
    """
    def hex(n \\ @default_length) when is_integer n do
      random_bytes(n)
      |> bitstring_to_list
      |> Enum.map(fn (x) -> integer_to_binary(x, 16) end)
      |> Enum.join
      |> String.downcase
    end
  
    @doc """
    Returns random bytes.
    
    ## Examples
  
    iex> T3.Utils.SecureRandom.random_bytes
    <<202, 104, 227, 197, 25, 7, 132, 73, 92, 186, 242, 13, 170, 115, 135, 7>>

    iex> T3.Utils.SecureRandom.random_bytes(8)
    <<231, 123, 252, 174, 156, 112, 15, 29>>
    """
    def random_bytes(n \\ @default_length) when is_integer n do
      :crypto.strong_rand_bytes(n)
    end
  end

end