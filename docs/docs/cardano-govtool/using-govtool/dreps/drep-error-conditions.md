# DRep error conditions

## There are three possible errors with DRep data

These errors arise when GovTool cannot find or cannot verify the data entered and stored by the DRep, at the time of the most recent registration or data change&#x20;

### Data Missing

#### The data that was originally used when this DRep was created has not been found.&#x20;

GovTool uses external sources for DRep data, and these sources are maintained by the DReps themselves. This error means that GovTool cannot locate the data on the URL specified when the DRep was originally registered.



### Data Not Verifiable

#### The data that was originally used when this DRep was created has changed.&#x20;

GovTool uses external sources for DRep data, and these sources are maintained by the DReps themselves. This error means that the data stored by the DRep does not match the data supplied by the DRep when they originally registered

### Data Formatted Incorrectly

#### The data that was originally used when this DRep was created has been formatted incorrectly.&#x20;

GovTool uses external sources for DRep data, and these sources are maintained by the DReps themselves. This error means that the data stored by the DRep does not match the format defined by the DRep spec.

## Errors during registration or update

When you register or update your DRep data, GovTool checks the URL you provide before submitting the transaction:

* **The URL You Entered Cannot Be Found**: GovTool could not download a file from the URL. Check that the URL is public and correct.
* **Your External Data Does Not Match the Original File.**: the file at the URL is not identical to the file you downloaded from GovTool. Upload the exact file again without modifying it.
