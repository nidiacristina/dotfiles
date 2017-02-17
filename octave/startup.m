function startup()
% startup()
%
% Initializes the Octave environment.  This loads a couple of packages that are
% commonly used.
%
% Takes no arguments.
%
% Returns nothing.
%

% packages we want to install.
%
%   hdf5oct - provides h5create(), h5read(), h5write() and friends.
%   signal  - provides tools for signal processing.
target_packages = { 'hdf5oct',
                    'signal' };

% load what we can.
load_available_packages( target_packages );

end

function [available_indices, unavailable_indices] = load_available_packges( target_packages )
% [available_indices, unavailable_indices] = load_available_packges( target_packages )
%
% Loads Octave packages if they're available.  Takes a cell of package names and
% loads each one that is installed on the system.  Each package that is not
% installed produces a warning.  Returns which packages were and were not
% loaded.
%
% Takes 1 argument:
%
%   target_packages - Cell array of package names to load if available.
%
% Returns 2 values:
%
%   available_indices   - Vector of indices in target_packages for packages that
%                         were loaded.
%   unavailable_indices - Vector of indices in target_packages for packages that
%                         were not loaded.
%

% packages that are available.
installed_packages = cellfun( @(x) x.name, pkg( 'list' ), ...
                              'UniformOutput', false );

% jump through some hoops to find each of the target packages installed on the
% system as a vector we can use as indices.
available_indices                = cellfun( @(x) strfind( installed_packages, x ), target_packages );
empty_indices                    = cellfun( @(x) isempty( x ), available_indices );

available_indices(empty_indices) = [];
available_indices                = [available_indices{:}];
unavailable_indices              = setdiff( 1:length( target_packages ), ...
                                            available_indices );

% load each of the packages we want and are available.
cellfun( @(x) pkg( 'load', x ), { target_packages{available_indices} } );

% warn about each of the packages that aren't available.
cellfun( @(x) warning( '''%s'' is not installed.  Skipping it''s load.', x ), ...
         { target_packages{unavailable_indices} } );

end
