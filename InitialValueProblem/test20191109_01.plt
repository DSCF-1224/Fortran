reset session

set key on           # {on|off} {default}
set key outside      # { {inside|outside} | {lmargin|rmargin|tmargin|bmargin} | {at <position>} }
set key right center # {left|right|center} {top|bottom|center}
set key vertical     # {vertical|horizontal}
set key Left         # {Left|Right}
set key reverse      # {noreverse|reverse} {noinvert|invert}
set key samplen 0    # {samplen <sample_length>} {spacing <vertical_spacing>}

path_fldr_data      = 'E:\Arc-2\test20191109_01'
path_file_data(var) = path_fldr_data . '\' . 'Euler' . sprintf('%2.2d', var) . '.bin'
str_binary_format   = '%3double'

set grid x
set grid y

set terminal wxt 0
set format   x '%3.1f'
set format   y '%3.1f'

plot \
	path_file_data(1) binary format=str_binary_format using 1:2 with points title 'h=1/02', \
	path_file_data(2) binary format=str_binary_format using 1:2 with points title 'h=1/04', \
	path_file_data(3) binary format=str_binary_format using 1:2 with points title 'h=1/08', \
	path_file_data(4) binary format=str_binary_format using 1:2 with points title 'h=1/16'

set terminal wxt 1
set logscale y 2          # <axes> {<base>}
set format   y '{2}^{%L}'

plot \
	path_file_data(1) binary format=str_binary_format using 1:3 with points title 'h=1/02', \
	path_file_data(2) binary format=str_binary_format using 1:3 with points title 'h=1/04', \
	path_file_data(3) binary format=str_binary_format using 1:3 with points title 'h=1/08', \
	path_file_data(4) binary format=str_binary_format using 1:3 with points title 'h=1/16'

# --- EOF --- #
