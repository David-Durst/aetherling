input_data = [i for i in range(1920*1080)]

def stencil_generator(row_size, inputs_2d):
  col_size = len(inputs_2d) // row_size
  num_rows = col_size
  num_cols = row_size
  def get_input(r, c):
      if ((r < 0) or (c < 0)):
        return 253
      else:
        return inputs_2d[r * row_size + c]
  return [
      [
          [
                253 if ((r - stencil_r < 0) or (c - stencil_c < 0)) else inputs_2d[(r - stencil_r) * row_size + (c - stencil_c)]
              for stencil_c in [2,1,0]
          ] for stencil_r in [2,1,0]
      ] for r in range(num_rows) for c in range(num_cols)
  ]

hask_kernel = [1,2,1,2,4,2,1,2,1]
def conv_generator(stencil_2d_output):
    result = []
    for window in stencil_2d_output:
        flat_window = window[0] + window[1] + window[2]
        if 253 in flat_window:
            result.append(253)
        else:
            mac = 0
            for i in range(9):
                mac += hask_kernel[i] * flat_window[i]
            result.append(mac % 256 // 16)
    return result


print(str(sum(conv_generator(stencil_generator(1920, [i for i in range(1920*1080)])))))
