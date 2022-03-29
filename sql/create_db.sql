CREATE TABLE "Job"
(
    id UUID PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    company TEXT NOT NULL,
    city TEXT NOT NULL,
    job_description TEXT NOT NULL,
    company_description TEXT NOT NULL,
    created_at TEXT NOT NULL,
    end_date TEXT NOT NULL,
    contact_email TEXT NOT NULL,
    contract_type TEXT NOT NULL,
    duration TEXT NOT NULL, 
    is_deleted BOOLEAN,
    ranking REAL
);

INSERT INTO "Job" (id, title, company, city, job_description, company_description, created_at, end_date, contact_email, contract_type, duration, is_deleted) 
VALUES  ("9241a0d5-2732-4331-a636-0dc195502c1d", "title", "company", "lille","job description", 
"company description", "29/03/2022", "30/03/2022", "qsi@cfini.fr", "CDI", "2 ans", FALSE);