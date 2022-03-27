CREATE TABLE "Job"
(
    id UUID PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    company TEXT NOT NULL,
    job_description TEXT NOT NULL,
    company_description TEXT NOT NULL,
    created_at DATE NOT NULL,
    end_date DATE NOT NULL,
    contact_email TEXT NOT NULL,
    contract_type TEXT NOT NULL,
    duration NUMBER NOT NULL, 
    is_deleted BOOLEAN,
    ranking NUMBER
)