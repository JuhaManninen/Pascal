{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit uConst;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface


const
  ProgName = 'Eldean ESS-Model';
  ProgVersion = '2.2';

  ProgMail = 'essmodel@eldean.se';
  ProgUrl  = 'http://www.essmodel.com';

  {$IFDEF TIMELIMIT}
  TimeLimitY = 2001;
  TimeLimitM = 5;
  TimeLimitD = 26;
  {$ENDIF}

  RegKey = 'Software\Eldean\EssModel';

implementation

end.