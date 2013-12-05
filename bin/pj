#!/usr/bin/env ruby
require 'rubygems'
require 'pp'
require 'multi_json'

def from_json(input)
  val = MultiJson.decode(input, symbolize_keys: true)
  case val
  when String
    puts val
  else
    p val
  end
end

input = ARGF.read
begin
  from_json(input)
rescue
  input.lines { |line| from_json(line) }
end
