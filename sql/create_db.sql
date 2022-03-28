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
    ranking REAL NOT NULL
);