## Loan Approval Example    
This folder contains the **WS-BPEL source file** and **QoS annotations file** (used by **PASO Analyser** to generate result) for the  **new Loan approval Example**. 
<br> <br> The main changes between **new Loan approval Example** w.r.t  **old Loan Request Example** are:

* Branch condition is based on “Short Term Loan” rather than “Big Amount”.

* For the new example, we took the values for response time and probability of success from WS-Dream and QWS public datasets. We bound: 
> InterestRate service with [MortgageIndex endpoint] (http://www.webservicex.net/MortgageIndex.asmx?WSDL) <br>
> Loan APR service with [FinanceService endpoint] (http://www.webservicex.com/FinanceService.asmx?WSDL)

* Fault handling behaviour is different. In new example, the orchestrator will wait for two seconds before trying again (as compared to changing endpoint in old Loan Request Example).
