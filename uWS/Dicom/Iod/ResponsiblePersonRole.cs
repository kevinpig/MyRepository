#region License

// Copyright (c) 2013, ClearCanvas Inc.
// All rights reserved.
// http://www.clearcanvas.ca
//
// This file is part of the ClearCanvas RIS/PACS open source project.
//
// The ClearCanvas RIS/PACS open source project is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// The ClearCanvas RIS/PACS open source project is distributed in the hope that it
// will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
// Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// the ClearCanvas RIS/PACS open source project.  If not, see
// <http://www.gnu.org/licenses/>.

#endregion

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Enumerated values for the <see cref="DicomTags.ResponsiblePersonRole"/> attribute indicating the role of the responsible person for the named patient.
	/// </summary>
	/// <remarks>
	/// As defined in the DICOM Standard 2009, Part 3, Section C.7.1.1.1.2
	/// </remarks>
	public enum ResponsiblePersonRole
	{
		/// <summary>
		/// None.
		/// </summary>
		None,

		/// <summary>
		/// Owner.
		/// </summary>
		Owner,

		/// <summary>
		/// Parent.
		/// </summary>
		Parent,

		/// <summary>
		/// Child.
		/// </summary>
		Child,

		/// <summary>
		/// Spouse.
		/// </summary>
		Spouse,

		/// <summary>
		/// Sibling.
		/// </summary>
		Sibling,

		/// <summary>
		/// Relative.
		/// </summary>
		Relative,

		/// <summary>
		/// Guardian.
		/// </summary>
		Guardian,

		/// <summary>
		/// Custodian.
		/// </summary>
		Custodian,

		/// <summary>
		/// Agent.
		/// </summary>
		Agent
	}
}