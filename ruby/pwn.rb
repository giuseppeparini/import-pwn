require 'socket'

TCPSocket.open('piecewise.challs.cyberchallenge.it',9110){|s|s.each{|line,_=puts(line)|s.write(line=~/^Please send me the number (\d+) as a (64|32)-bit (big|little)-endian /?[$1.to_i].pack(({32=>'L',64=>'Q'}[$2.to_i])+({big:'>',little:'<'}[$3.to_sym])):"\n")&&puts(s.gets)}}
