CREATE TABLE "Job"
(
    id UUID PRIMARY KEY NOT NULL,
    title TEXT,
    company TEXT,
    job_description TEXT,
    company_description TEXT,
    created_at DATE,
    end_at DATE,
    contact_email TEXT,
    job_type TEXT,
    duration NUMBER, 
    is_deleted BOOLEAN,
    ranking NUMBER
)