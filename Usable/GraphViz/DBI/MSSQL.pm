package GraphViz::DBI::MSSQL;
use GraphViz::DBI;
our @ISA; BEGIN { @ISA = qw/GraphViz::DBI/; }
sub get_tables {
	my $self = shift;
	my $dbh = $self->get_dbh;
	my $tabs = $dbh->selectcol_arrayref('select table_name from information_schema.tables where table_type = ?', undef, 'BASE TABLE');
	@$tabs;
}
sub _init_fks {
	my $self = shift;
	return if exists $$self{_fk_info};
	$$self{_fk_info} = {};
	my $dbh = $self->get_dbh;
	my $fks = $dbh->selectall_arrayref(<<SQL);
select
	fk.table_name as from_table,
	fkc.column_name as from_column,
	pk.table_name as to_table,
	pkc.column_name as to_column
from
    information_schema.referential_constraints as c
    inner join information_schema.table_constraints as fk
        on c.constraint_name = fk.constraint_name
    inner join information_schema.table_constraints as pk
        on c.unique_constraint_name = pk.constraint_name
	inner join information_schema.constraint_column_usage as fkc
		on fkc.constraint_name = fk.constraint_name
	inner join information_schema.constraint_column_usage as pkc
		on pkc.constraint_name = pk.constraint_name
SQL
	for (@$fks) {
		my ($ftab, $fcol, $ttab, $tcol) = @$_;
		$$self{_fk_info}{$ftab}{$fcol}{$ttab}{$tcol}++;
	}
}
sub is_foreign_key {
	my ($self, $table, $column) = @_;
	$self->_init_fks;
	my $fk = $$self{_fk_info};
	return unless exists $$fk{$table};
	$fk = $$fk{$table};
	return unless exists $$fk{$column};
	$fk = $$fk{$column};
	my ($to_table) = keys %$fk;
	return map {; $to_table => $_ } map keys %$_, values %$fk;
}
sub graph_tables {
	my $self = shift;
	my $just_tab = @_ ? 1 : 0;
	my %table = map {; $_ => 1 } $self->get_tables;
	my $dbh = $self->get_dbh;
	my @edges;
	my $ret = <<HEAD;
digraph test {
	graph [ratio=fill];
HEAD
	for my $table ($self->get_tables) {
		my $sth = $dbh->prepare("select * from ".($dbh->quote_identifier($table))." where 1 = 0");
		$sth->execute;
		my @cols = @{$$sth{NAME}};
		$sth->finish;
		if ($just_tab) {
			$ret .= "\tnode $table;\n";
		} else {
			$ret .= "\t$table [shape=record,label=\"{$table|";
			$ret .= join '|', map "<$_>$_", @cols;
			$ret .= "}\"];\n";
		}
		for my $col (@cols) {
			if (my ($ttab, $tcol) = $self->is_foreign_key($table, $col)) {
				push @edges, [ $table, $col, $ttab, $tcol ];
			}
		}
	}
	for (@edges) {
		my ($table, $col, $ttab, $tcol) = @$_;
		if ($just_tab) {
			$ret .= "\t$table -> $ttab;\n";
		} else {
			$ret .= "\t$table\:$col\:e -> $ttab\:$tcol\:w;\n";
		}
	}
	$ret . <<TAIL;
}
TAIL
}
1;
