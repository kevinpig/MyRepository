#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Collections.Generic;
using uWS.Dicom.Iod.Macros;
using uWS.Dicom.Iod.Macros.HierarchicalSeriesInstanceReference;

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Helper class to quickly build a hierarchical sequence of
	/// <see cref="IHierarchicalSopInstanceReferenceMacro"/>s with minimal repetition.
	/// </summary>
	public class HierarchicalSopInstanceReferenceDictionary
	{
		private readonly Dictionary<string, Dictionary<string, Dictionary<string, string>>> _dictionary = new Dictionary<string, Dictionary<string, Dictionary<string, string>>>();

		/// <summary>
		/// Constructs an instance of a hierarchical SOP reference dictionary.
		/// </summary>
		public HierarchicalSopInstanceReferenceDictionary() {}

		/// <summary>
		/// Adds a SOP reference to the dictionary.
		/// </summary>
		/// <param name="studyInstanceUid">The study instance UID.</param>
		/// <param name="seriesInstanceUid">The series instance UID.</param>
		/// <param name="sopClassUid">The SOP class UID.</param>
		/// <param name="sopInstanceUid">The SOP instance UID.</param>
		/// <exception cref="ArgumentNullException">Thrown if any of the arguments are null or empty.</exception>
		/// <exception cref="ArgumentException">Thrown if that SOP instance has already been added to the dictionary.</exception>
		/// <remarks>
		/// The dictionary does not allow for redefining the SOP class for a given instance using this method.
		/// To do so, the particular SOP instance should be removed first. A SOP instance can only be referenced as
		/// one SOP class.
		/// </remarks>
		public virtual void AddReference(string studyInstanceUid, string seriesInstanceUid, string sopClassUid, string sopInstanceUid)
		{
			bool result = TryAddReference(studyInstanceUid, seriesInstanceUid, sopClassUid, sopInstanceUid);
			if (!result)
				throw new ArgumentException("That SOP Instance has already been added to the dictionary.", "sopInstanceUid");
		}

		/// <summary>
		/// Adds a SOP reference to the dictionary.
		/// </summary>
		/// <param name="studyInstanceUid">The study instance UID.</param>
		/// <param name="seriesInstanceUid">The series instance UID.</param>
		/// <param name="sopClassUid">The SOP class UID.</param>
		/// <param name="sopInstanceUid">The SOP instance UID.</param>
		/// <exception cref="ArgumentNullException">Thrown if any of the arguments are null or empty.</exception>
		/// <returns>True if the SOP was referenced successfully; False if a reference already exists for the given SOP instance.</returns>
		public virtual bool TryAddReference(string studyInstanceUid, string seriesInstanceUid, string sopClassUid, string sopInstanceUid)
		{
			if (string.IsNullOrEmpty(studyInstanceUid))
				throw new ArgumentNullException("studyInstanceUid");
			if (string.IsNullOrEmpty(seriesInstanceUid))
				throw new ArgumentNullException("seriesInstanceUid");
			if (string.IsNullOrEmpty(sopClassUid))
				throw new ArgumentNullException("sopClassUid");
			if (string.IsNullOrEmpty(sopInstanceUid))
				throw new ArgumentNullException("sopInstanceUid");

			if (!_dictionary.ContainsKey(studyInstanceUid))
				_dictionary.Add(studyInstanceUid, new Dictionary<string, Dictionary<string, string>>());
			Dictionary<string, Dictionary<string, string>> seriesDictionary = _dictionary[studyInstanceUid];

			if (!seriesDictionary.ContainsKey(seriesInstanceUid))
				seriesDictionary.Add(seriesInstanceUid, new Dictionary<string, string>());
			Dictionary<string, string> sopDictionary = seriesDictionary[seriesInstanceUid];

			if (sopDictionary.ContainsKey(sopInstanceUid))
				return false;

			sopDictionary.Add(sopInstanceUid, sopClassUid);
			return true;
		}

		/// <summary>
		/// Attempts to remove the given SOP reference from the dictionary.
		/// </summary>
		/// <param name="studyInstanceUid">The study instance UID.</param>
		/// <param name="seriesInstanceUid">The series instance UID.</param>
		/// <param name="sopClassUid">The SOP class UID.</param>
		/// <param name="sopInstanceUid">The SOP instance UID.</param>
		/// <exception cref="ArgumentNullException">Thrown if any of the arguments are null or empty.</exception>
		/// <returns>True if the SOP was unreferenced successfully; False if a reference does not already exist for the given SOP instance.</returns>
		public virtual bool TryRemoveReference(string studyInstanceUid, string seriesInstanceUid, string sopClassUid, string sopInstanceUid)
		{
			if (string.IsNullOrEmpty(studyInstanceUid))
				throw new ArgumentNullException("studyInstanceUid");
			if (string.IsNullOrEmpty(seriesInstanceUid))
				throw new ArgumentNullException("seriesInstanceUid");
			if (string.IsNullOrEmpty(sopClassUid))
				throw new ArgumentNullException("sopClassUid");
			if (string.IsNullOrEmpty(sopInstanceUid))
				throw new ArgumentNullException("sopInstanceUid");

			if (_dictionary.ContainsKey(studyInstanceUid))
			{
				Dictionary<string, Dictionary<string, string>> seriesDictionary = _dictionary[studyInstanceUid];
				if (seriesDictionary.ContainsKey(seriesInstanceUid))
				{
					Dictionary<string, string> sopDictionary = seriesDictionary[seriesInstanceUid];
					if (sopDictionary.ContainsKey(sopInstanceUid))
					{
						if (sopDictionary[sopInstanceUid] == sopClassUid)
						{
							sopDictionary.Remove(sopInstanceUid);

							// compact the dictionary
							if (sopDictionary.Count == 0)
							{
								seriesDictionary.Remove(seriesInstanceUid);
								if (seriesDictionary.Count == 0)
								{
									_dictionary.Remove(studyInstanceUid);
								}
							}
							return true;
						}
					}
				}
			}
			return false;
		}

		/// <summary>
		/// Clears the reference dictionary.
		/// </summary>
		public virtual void Clear()
		{
			_dictionary.Clear();
		}

		/// <summary>
		/// Creates and initializes a <see cref="IHierarchicalSopInstanceReferenceMacro"/> to the given study instance.
		/// </summary>
		/// <param name="studyInstanceUid">The study instance UID.</param>
		protected virtual IHierarchicalSopInstanceReferenceMacro CreateStudyReference(string studyInstanceUid)
		{
			IHierarchicalSopInstanceReferenceMacro reference = new HierarchicalSopInstanceReferenceMacro(new DicomSequenceItem());
			reference.InitializeAttributes();
			reference.StudyInstanceUid = studyInstanceUid;
			return reference;
		}

		/// <summary>
		/// Creates and initializes a <see cref="IHierarchicalSeriesInstanceReferenceMacro"/> to the given series instance.
		/// </summary>
		/// <param name="seriesInstanceUid">The series instance UID.</param>
		protected virtual IHierarchicalSeriesInstanceReferenceMacro CreateSeriesReference(string seriesInstanceUid)
		{
			IHierarchicalSeriesInstanceReferenceMacro reference = new HierarchicalSeriesInstanceReferenceMacro(new DicomSequenceItem());
			reference.InitializeAttributes();
			reference.SeriesInstanceUid = seriesInstanceUid;
			return reference;
		}

		/// <summary>
		/// Creates and initializes a <see cref="IReferencedSopSequence"/> to the given SOP instance.
		/// </summary>
		/// <param name="sopClassUid">The SOP class UID.</param>
		/// <param name="sopInstanceUid">The SOP instance UID.</param>
		protected virtual IReferencedSopSequence CreateSopReference(string sopClassUid, string sopInstanceUid)
		{
			IReferencedSopSequence reference = new HierarchicalSeriesInstanceReferenceMacro.ReferencedSopSequenceType(new DicomSequenceItem());
			reference.InitializeAttributes();
			reference.ReferencedSopClassUid = sopClassUid;
			reference.ReferencedSopInstanceUid = sopInstanceUid;
			return reference;
		}

		/// <summary>
		/// Gets the references as an array of <see cref="IHierarchicalSopInstanceReferenceMacro"/>s.
		/// </summary>
		public IHierarchicalSopInstanceReferenceMacro[] ToArray()
		{
			return this.GetList().ToArray();
		}

		/// <summary>
		/// Gets the references as a readonly <see cref="IList{T}"/> of <see cref="IHierarchicalSopInstanceReferenceMacro"/>s.
		/// </summary>
		public IList<IHierarchicalSopInstanceReferenceMacro> ToList()
		{
			return this.GetList().AsReadOnly();
		}

		private List<IHierarchicalSopInstanceReferenceMacro> GetList()
		{
			List<IHierarchicalSopInstanceReferenceMacro> studyReferences = new List<IHierarchicalSopInstanceReferenceMacro>();
			foreach (KeyValuePair<string, Dictionary<string, Dictionary<string, string>>> studyPair in _dictionary)
			{
				IHierarchicalSopInstanceReferenceMacro studyReference = this.CreateStudyReference(studyPair.Key);

				List<IHierarchicalSeriesInstanceReferenceMacro> seriesReferences = new List<IHierarchicalSeriesInstanceReferenceMacro>();
				foreach (KeyValuePair<string, Dictionary<string, string>> seriesPair in studyPair.Value)
				{
					IHierarchicalSeriesInstanceReferenceMacro seriesReference = this.CreateSeriesReference(seriesPair.Key);

					List<IReferencedSopSequence> sopReferences = new List<IReferencedSopSequence>();
					foreach (KeyValuePair<string, string> sopPair in seriesPair.Value)
					{
						IReferencedSopSequence sopReference = this.CreateSopReference(sopPair.Value, sopPair.Key);
						sopReferences.Add(sopReference);
					}

					seriesReference.ReferencedSopSequence = sopReferences.ToArray();
					seriesReferences.Add(seriesReference);
				}

				studyReference.ReferencedSeriesSequence = seriesReferences.ToArray();
				studyReferences.Add(studyReference);
			}

			return studyReferences;
		}

		/// <summary>
		/// Gets the specified reference <paramref name="dictionary"/> as an array of <see cref="IHierarchicalSopInstanceReferenceMacro"/>s.
		/// </summary>
		public static implicit operator IHierarchicalSopInstanceReferenceMacro[] (HierarchicalSopInstanceReferenceDictionary dictionary)
		{
			return dictionary.ToArray();
		}
	}
}