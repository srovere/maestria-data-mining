#!/usr/bin/perl

use strict; 
use Log::Log4perl;
use YAML::XS;

# 1. Definicion de funciones
sub leerArchivoConfiguracion($$) {
	my ($config_file, $logger) = @_;
	if (not (-f $config_file)) {
		$logger->fatal("No se puede abrir el archivo de configuracion $config_file");
		exit(1);
	}
	my $config;
	eval {
		$config = YAML::XS::LoadFile($config_file);
	};
	if ($@) {
		$logger->fatal("Error al leer el archivo de configuracion $config_file: $@");
		exit(1);
	} else {
		$logger->info("Configuracion leida de $config_file");
		return $config;
	}
}

sub listFiles($$$$;$) {
	my ($base, $config, $files, $logger, $paths) = @_;

	$paths ||= {};
	foreach my $file (keys(%$files)) {
		my $value = $files->{$file};
		if ('HASH' eq ref($value)) {
			# Es un directorio
			my $dir = $base . '/' . $file;
			listFiles($dir, $config, $files->{$file}, $logger, $paths);
		} else {
			# Es un archivo
			my $path_attributes = {
				source	=>	($base . '/' . $file . '.tmpl'),
				target	=>	($base . '/' . $file)
			};
			if ($value) {
				# Es un archivo con atributos
				my @keyvals = split(/,/, $value);
				foreach my $keyval (@keyvals) {
					if ($keyval =~ /^(.+?)=(.+?)$/) {
						my ($key, $val) = ($1, $2);
						if ($key eq 'version') {
							$path_attributes->{source} = ($base . '/' . $file . '-' . $config->{$val}{version} . '.tmpl');
						} elsif ($key eq 'mode') {
							$path_attributes->{mode} = $val;
						}
					} else {
						$logger->warn("Par atributo/valor desconocido: $keyval");
					}
				}
			}

			if (not (-f $path_attributes->{source})) {
				$logger->warn("El archivo " . $path_attributes->{source} . " no existe");
			} else {
				$paths->{$path_attributes->{source}} = $path_attributes;
			}
		}
	}

	return $paths;
}

sub getConfigValue($$) {
	my ($config, $keys) = @_;
	my $key   = shift(@$keys);
	my $value = $config->{$key};
	if (@$keys) {
		return getConfigValue($value, $keys);
	} else {
		return $value;
	}
}

sub createFile($$$$) {
	my ($config, $source, $target, $logger) = @_;
	$logger->info("Generando archivo $target");

	# a. Leer el source
	my $sourceContent;
	{
		local $/;
		if (not open(my $sfh, '<:encoding(UTF-8)', $source)) {
			$logger->warn("... No se puede abrir el archivo para lectura $source");
			return;
		} else {
			$sourceContent = <$sfh>;
			close($sfh);
		}
	}
 
	# b. Efectuar reemplazos de placeholders
	my $TIMEOUT = 10;
	my $targetContent = $sourceContent;
	eval {
		local $SIG{ALRM} = sub {
			die("Tardo mas de $TIMEOUT segundos en generar el archivo $target. Abortando generacion");
		};
		alarm($TIMEOUT);
		while ($targetContent =~ m!\$\{((\w+)(\.(\w+))*)\}!) {
			my $keys        = [ split(/\./, $1) ];
			my $placeholder = '\\$\\{' . $1 . '\\}';
			my $replacement = getConfigValue($config, $keys);
			if ($replacement) {
				$targetContent =~ s/$placeholder/$replacement/ge;
			} else {
				$logger->error('... No hay variable de reemplazo definida para ${' . $1 . '}');
				$targetContent =~ s/$placeholder/$replacement/ge;
			}
		} 
		alarm 0;
	};
	if ($@) {
		# Dio timeout
		$logger->error($@);
		return;
	}

	# c. Generar nuevo archivo con contenido especifico
	if (not open(my $tfh, '>:encoding(UTF-8)', $target)) {
		$logger->error("No se puede abrir el archivo para escritura $target");
		return;
	} else {
		print $tfh $targetContent;
		close($tfh);
	}
}

{
	# 2. Inicializar logger
	my $conf = {
		'log4perl.category.SIAT.Installer'					=>	"INFO, ScreenColoredLevels",
		'log4perl.appender.ScreenColoredLevels'					=>	'Log::Log4perl::Appender::Screen',
		'log4perl.appender.ScreenColoredLevels.layout'				=>	"Log::Log4perl::Layout::PatternLayout",
		'log4perl.appender.ScreenColoredLevels.layout.ConversionPattern'	=>	"%d - (%P) - %p - %m%n"
	};
	Log::Log4perl->init($conf);
	my $logger = Log::Log4perl->get_logger("SIAT::Installer");

	# 3. Determinar modo de ejecucion
	my $mode = $ARGV[0];
	if (($mode ne 'i') and ($mode ne 'd')) {
		$logger->fatal("Modo de ejecucion: i = instalar, d = desinstalar");
		exit(1);
	}

	# 4. Leer archivos de configuracion
	my $config = leerArchivoConfiguracion($ARGV[1], $logger);
	$config->{files} = leerArchivoConfiguracion('archivos.yml', $logger);
	$config->{mode} = $mode;

	# 5. Buscar todos los archivos a reemplazar 
	my $base  = $config->{base};
	my $files = $config->{files};
	my $paths = listFiles($base, $config, $files, $logger);

	# 6. Efectuar las acciones uno por uno
	foreach my $path(sort(keys(%$paths))) {
		my $path_info = $paths->{$path};

		if ($config->{mode} eq 'i') {
			# a. Crear el archivo
			createFile($config, $path_info->{source}, $path_info->{target}, $logger);

			# b. Setear permisos si es necesario
			if ($path_info->{mode}) {
				$logger->info("Seteando modo " . $path_info->{mode} . " en " . $path_info->{target});
				if (not chmod(oct($path_info->{mode}), $path_info->{target})) {
					$logger->warn("No se pudo cambiar el modo de " . $path_info->{target} . " a " . $path_info->{mode});
				}
			}
		} elsif ($config->{mode} eq 'd') {
			# Borrar archivo
			$logger->info("Borrando " . $path_info->{target});
			unlink($path_info->{target});
		}
	}
}
