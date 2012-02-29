#!/usr/bin/env ruby

require 'redcarpet'

begin
	require 'pygments'
	$colors = 'pygments'
rescue LoadError
	require 'albino'
	$colors = 'albino'
end

require 'cgi'
require 'optparse'
opts = {
	:additions => true,
	:github => false,
}
optparse = OptionParser.new do |o|
	o.on('--no-additions') { opts[:additions] = false }
	o.on('--github-style') { opts[:github] = true }
end
optparse.parse!(ARGV)

def cgi? ; ENV['GATEWAY_INTERFACE'] ; end

cgi = CGI.new('html3')

def h(*args)
	CGI::escapeHTML(*args)
end

if !cgi['source'].empty?
	print cgi.pre { h ARGF.read }
	exit
end

def title
	begin
		File.basename(ENV['SCRIPT_FILENAME'], '.markdown')
	rescue
		'no filename'
	end
end

puts <<HEADER
<!DOCTYPE html>
<html>
<head>
<title>#{h title}</title>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"></script>
<script src="/jquery.plugin.loader.js"></script>
<script>
jQuery.noConflict();
jQueryPluginLoader({name:'markdown-craziness',css:true});
</script>
HEADER

case $colors
when 'albino'
	puts <<CSS
<style type="text/css">
/* CSS for Albino highlighter */
.hll { background-color: #ffffcc }
.c { color: #408080; font-style: italic } /* Comment */
.err { border: 1px solid #FF0000 } /* Error */
.k { color: #008000; font-weight: bold } /* Keyword */
.o { color: #666666 } /* Operator */
.cm { color: #408080; font-style: italic } /* Comment.Multiline */
.cp { color: #BC7A00 } /* Comment.Preproc */
.c1 { color: #408080; font-style: italic } /* Comment.Single */
.cs { color: #408080; font-style: italic } /* Comment.Special */
.gd { color: #A00000 } /* Generic.Deleted */
.ge { font-style: italic } /* Generic.Emph */
.gr { color: #FF0000 } /* Generic.Error */
.gh { color: #000080; font-weight: bold } /* Generic.Heading */
.gi { color: #00A000 } /* Generic.Inserted */
.go { color: #808080 } /* Generic.Output */
.gp { color: #000080; font-weight: bold } /* Generic.Prompt */
.gs { font-weight: bold } /* Generic.Strong */
.gu { color: #800080; font-weight: bold } /* Generic.Subheading */
.gt { color: #0040D0 } /* Generic.Traceback */
.kc { color: #008000; font-weight: bold } /* Keyword.Constant */
.kd { color: #008000; font-weight: bold } /* Keyword.Declaration */
.kn { color: #008000; font-weight: bold } /* Keyword.Namespace */
.kp { color: #008000 } /* Keyword.Pseudo */
.kr { color: #008000; font-weight: bold } /* Keyword.Reserved */
.kt { color: #B00040 } /* Keyword.Type */
.m { color: #666666 } /* Literal.Number */
.s { color: #BA2121 } /* Literal.String */
.na { color: #7D9029 } /* Name.Attribute */
.nb { color: #008000 } /* Name.Builtin */
.nc { color: #0000FF; font-weight: bold } /* Name.Class */
.no { color: #880000 } /* Name.Constant */
.nd { color: #AA22FF } /* Name.Decorator */
.ni { color: #999999; font-weight: bold } /* Name.Entity */
.ne { color: #D2413A; font-weight: bold } /* Name.Exception */
.nf { color: #0000FF } /* Name.Function */
.nl { color: #A0A000 } /* Name.Label */
.nn { color: #0000FF; font-weight: bold } /* Name.Namespace */
.nt { color: #008000; font-weight: bold } /* Name.Tag */
.nv { color: #19177C } /* Name.Variable */
.ow { color: #AA22FF; font-weight: bold } /* Operator.Word */
.w { color: #bbbbbb } /* Text.Whitespace */
.mf { color: #666666 } /* Literal.Number.Float */
.mh { color: #666666 } /* Literal.Number.Hex */
.mi { color: #666666 } /* Literal.Number.Integer */
.mo { color: #666666 } /* Literal.Number.Oct */
.sb { color: #BA2121 } /* Literal.String.Backtick */
.sc { color: #BA2121 } /* Literal.String.Char */
.sd { color: #BA2121; font-style: italic } /* Literal.String.Doc */
.s2 { color: #BA2121 } /* Literal.String.Double */
.se { color: #BB6622; font-weight: bold } /* Literal.String.Escape */
.sh { color: #BA2121 } /* Literal.String.Heredoc */
.si { color: #BB6688; font-weight: bold } /* Literal.String.Interpol */
.sx { color: #008000 } /* Literal.String.Other */
.sr { color: #BB6688 } /* Literal.String.Regex */
.s1 { color: #BA2121 } /* Literal.String.Single */
.ss { color: #19177C } /* Literal.String.Symbol */
.bp { color: #008000 } /* Name.Builtin.Pseudo */
.vc { color: #19177C } /* Name.Variable.Class */
.vg { color: #19177C } /* Name.Variable.Global */
.vi { color: #19177C } /* Name.Variable.Instance */
.il { color: #666666 } /* Literal.Number.Integer.Long */
</style>
CSS
when 'pygments'
	if opts[:github]
		puts <<CSS
<link rel="stylesheet" type="text/css" href="https://a248.e.akamai.net/assets.github.com/stylesheets/bundles/github-c2f83c57a14d5e54d816673f52563d987fe2821f.css">
<link rel="stylesheet" type="text/css" href="https://a248.e.akamai.net/assets.github.com/stylesheets/bundles/github2-34d96ae148c427d3106177152ac475d7df36c780.css">
CSS
	else
		puts <<CSS
<style type="text/css">
/* CSS for Pygments highlighter */
#{Pygments.css :style => 'vs'}
</style>
CSS
	end
end

puts '<a class="source-link" href="?source=1">Markdown source</a>' if opts[:additions]

puts <<HEADER
</head>
<body#{opts[:github] ? ' class="markdown-body"' : ''}>
HEADER

class HighlightedHTML < Redcarpet::Render::HTML
	def block_code(code, language)
		begin
			case $colors
			when 'pygments'
				Pygments.highlight(code, :lexer => language)
			when 'albino'
				Albino.safe_colorize(code, language)
			else
				throw 'fail'
			end
		rescue
			"<!-- #{$colors} failed to render: #{language} --><pre>#{code}</pre>"
		end
	end
end

begin
	markdown = Redcarpet::Markdown.new(HighlightedHTML, :fenced_code_blocks => true)
	content = ARGF.read
	puts markdown.render(content)
rescue Exception => e
	content ||= "(unknown)"
	puts <<FAIL
Failed to render Markdown.
Error:
#{e}
Content:
#{content}
FAIL
end

puts <<FOOTER
</body>
</html>
FOOTER
