(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-index-malformed ()
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/no_review_data.org")))))
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/no_properties.org")))))
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/normal_swapped_drawers.org")))))
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/unclosed_drawer1.org")))))
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/unclosed_drawer2.org"))))))

(ert-deftest org-fc-test-escaping ()
  (let ((index (org-fc-awk-index-paths
                (list (org-fc-test-fixture "escaping/spaces in filename.org")))))
    (should (eq (length index) 1))
    (should
     (equal (plist-get (car index) :id)
            "33645f3a-384d-44ed-aed2-a2d56b973800"))))

(ert-deftest org-fc-test-index-keywords ()
  (let ((index (org-fc-awk-index-paths
                (list (org-fc-test-fixture "index/uppercase.org")))))
    (should (eq (length index) 1))
    (let ((card (car index)))
      (should (equal (plist-get card :inherited-tags) ":tag1:tag2:"))
      (should (equal (plist-get card :filetitle) "File Title"))))
  (let ((index (org-fc-awk-index-paths
                (list (org-fc-test-fixture "index/lowercase.org")))))
    (should (eq (length index) 1))
    (let ((card (car index)))
      (should (equal (plist-get card :inherited-tags) ":tag1:tag2:"))
      (should (equal (plist-get card :filetitle) "File Title")))))

(ert-deftest org-fc-test-index ()
  (let ((index (org-fc-awk-index-paths
                (list
                 (org-fc-test-fixture "index/test.org")))))
    (should (eq (length index) 3))
    (let ((card1 (car index))
          (card2 (cadr index))
          (card3 (caddr index)))
      (should
       (equal (plist-get card1 :id)
              "edee8940-5c9a-4c70-b1c4-f45c194c0c97"))
      (should
       (equal (plist-get card1 :local-tags)
              ":fc:tag1:"))
      (should
       (equal (plist-get card1 :title)
              "Headline"))

      (should
       (equal (plist-get card2 :id)
              "59b3b102-aebd-44ba-a1fd-6dc912c34fcf"))
      (should
       (equal (plist-get card2 :local-tags)
              ":fc:tag2:"))
      (should
       (equal (plist-get card2 :title)
              "Headline 2"))

      (should
       (equal (plist-get card3 :id)
              "a7ed2686-73e6-4780-825d-78cf4b2e5374"))
      (should
       (equal (plist-get card3 :local-tags)
              ":fc:tag3:"))
      (should
       (equal (plist-get card3 :title)
              "Headline 3:not_a_tag:")))))

(ert-deftest org-fc-test-index-no-heading ()
  (let ((index (org-fc-awk-index-paths
                (list
                 (org-fc-test-fixture "index/test_cards_no_heading.org")))))
    (should (eq (length index) 1))
    (let ((card-with-heading1 (car index)))
      (should
       (equal (plist-get card-with-heading1 :id)
              "b8ff9a29-781c-11ec-88e9-00d861145941"))
      (should
       (equal (plist-get card-with-heading1 :tags)
              '("fc" "suspended")))
      (should
       (equal (plist-get card-with-heading1 :filetitle)
              "What is Lorem Ipsum?")))))

(ert-deftest org-fc-test-index-mixed ()
  (let ((index (org-fc-awk-index-paths
                (list
                 (org-fc-test-fixture "index/test_cards_mixed.org")))))
    (should (eq (length index) 3))
    (let ((card-no-heading (car index))
          (card-with-heading1 (cadr index))
          (card-with-heading2 (caddr index)))
      (should
       (equal (plist-get card-no-heading :id)
              "bc004718-7870-11ec-88e9-00d861145941"))
      (should
       (equal (plist-get card-no-heading :tags)
              '("fc")))
      (should
       (equal (plist-get card-no-heading :filetitle)
              "No Headline 1"))
      
      (should
       (equal (plist-get card-with-heading1 :id)
              "edee8940-5c9a-4c70-b1c4-f45c194c0c97"))
      (should
       (equal (plist-get card-with-heading1 :local-tags)
              ":fc:tag1:"))
      (should
       (equal (plist-get card-with-heading1 :title)
              "Headline 1"))

      (should
       (equal (plist-get card-with-heading2 :id)
              "59b3b102-aebd-44ba-a1fd-6dc912c34fcf"))
      (should
       (equal (plist-get card-with-heading2 :local-tags)
              ":fc:tag2:"))
      (should
       (equal (plist-get card-with-heading2 :title)
              "Headline 2")))))
