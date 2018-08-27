# Minorly-enhanced URI class.
#
# Adds:
# - support for SSH scheme
# - `#to_h`
require 'uri'
require 'socket'

module URI
  class << self
    def add_scheme(name, default_port=nil)
      uc = name.to_s.upcase
      sym = uc.to_sym
      unless default_port.is_a?(Integer)
        default_port = Socket.getservbyname(uc.downcase) rescue nil
      end
      @@schemes[uc] = const_set(sym, Class.new(Generic)).class_eval do
        const_set('DEFAULT_PORT', default_port) if default_port
        self
      end
    end
  end

  add_scheme :postgresql
  add_scheme :ssh

  def fields
    component + %I[user password default_port] - %I[userinfo]
  end

  def to_h
    fields.map { |k| [k, send(k)] }.to_h
  end
end
